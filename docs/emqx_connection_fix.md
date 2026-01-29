## EMQX连接问题诊断和解决方案

### 问题现象
启动应用后，EMQX连接日志显示session已建立，但：
1. 没有看到"Subscribed to topic successfully"日志
2. 服务端看不到连接成功
3. 消息窗口不打印消息

### 根本原因分析

从日志分析：
```
2026-01-29 17:13:09.062 [info] EMQX session established: client_id=eadm_17696779890526992
2026-01-29 17:13:09.062 [info] Subscribing to topic: /tracker/864269060008576/up/stat
```

1. **emqttc库的API问题**: 代码中调用 `emqttc:subscribe(Client, TopicBinary, 1)` 期望返回 `{ok, _Props, _ReasonCodes}`，但emqttc实际返回格式可能不同

2. **消息接收回调格式不匹配**: `handle_info({mqtt_message, Topic, Payload}, State)` 可能不是emqttc发送的实际消息格式

3. **缺少SSL配置**: state record中没有ssl和ssl_opts字段，但配置文件中有SSL配置

### 解决方案

#### 方案1：最小改动 - 修复订阅和消息接收

修改 `src/service/emqx_sync_service.erl` 的三个地方：

**1. 修改state record添加SSL支持（第24-32行）：**
```erlang
-record(state, {
    emqx_host,
    emqx_port,
    emqx_username,
    emqx_password,
    emqx_client_id,
    emqx_topic,
    emqx_ssl,        % 添加
    emqx_ssl_opts,   % 添加
    emqx_client
}).
```

**2. 修改init函数读取SSL配置（第60-95行）：**
```erlang
init([]) ->
    lager:info("Starting EMQX sync service..."),
    
    %% 从emqx应用环境读取pools配置
    case application:get_env(emqx, pools) of
        {ok, EmqxPools} ->
            {pool_emqx, _EmqxPoolOpts, EmqxConnOpts} = hd(EmqxPools),
            
            EmqxHost = proplists:get_value(host, EmqxConnOpts),
            EmqxPort = proplists:get_value(port, EmqxConnOpts),
            EmqxUsername = proplists:get_value(username, EmqxConnOpts),
            EmqxPassword = proplists:get_value(password, EmqxConnOpts),
            EmqxTopic = proplists:get_value(topic, EmqxConnOpts),
            EmqxSsl = proplists:get_value(ssl, EmqxConnOpts, false),  % 添加
            EmqxSslOpts = proplists:get_value(ssl_opts, EmqxConnOpts, []),  % 添加
            EmqxClientIdPrefix = proplists:get_value(client_id_prefix, EmqxConnOpts, "eadm_"),
            EmqxClientId = iolist_to_binary([EmqxClientIdPrefix, integer_to_binary(erlang:system_time())]),
            
            State = #state{
                emqx_host = EmqxHost,
                emqx_port = EmqxPort,
                emqx_username = EmqxUsername,
                emqx_password = EmqxPassword,
                emqx_topic = EmqxTopic,
                emqx_ssl = EmqxSsl,           % 添加
                emqx_ssl_opts = EmqxSslOpts,  % 添加
                emqx_client_id = EmqxClientId
            },
            
            %% 连接EMQX
            case connect_emqx(State) of
                {ok, EmqxClient} ->
                    lager:info("EMQX connected: client_id=~s ssl=~p", [EmqxClientId, EmqxSsl]),
                    {ok, State#state{emqx_client = EmqxClient}};
                {error, Reason} ->
                    lager:error(\"Failed to connect to EMQX: ~p", [Reason]),
                    %% 不要立即停止，稍后重连
                    erlang:send_after(5000, self(), reconnect),
                    {ok, State#state{emqx_client = undefined}}
            end;
        undefined ->
            lager:error("EMQX config not found"),
            {ok, #state{emqx_client = undefined}}
    end.
```

**3. 修改connect_emqx添加SSL和修正订阅（第163-202行）：**
```erlang
connect_emqx(#state{
    emqx_host = Host,
    emqx_port = Port,
    emqx_username = Username,
    emqx_password = Password,
    emqx_client_id = ClientId,
    emqx_topic = Topic,
    emqx_ssl = SslEnabled,      % 添加
    emqx_ssl_opts = SslOpts     % 添加
}) ->
    lager:info("Connecting to EMQX: ~s:~p ssl=~p", [Host, Port, SslEnabled]),
    
    BaseOptions = [
        {host, Host},
        {port, Port},
        {username, Username},
        {password, Password},
        {clientid, ClientId},
        {clean_sess, true},
        {keepalive, 60},
        {logger, info}
    ],
    
    %% 根据SSL配置添加选项
    Options = case SslEnabled of
        true ->
            Base Options ++ [{ssl, SslOpts}];
        _ ->
            BaseOptions
    end,
    
    case emqttc:start_link(Options) of
        {ok, Client} ->
            lager:info("EMQX client started, waiting for connection..."),
            timer:sleep(500),  % 等待连接建立
            
            %% 订阅设备数据主题
            TopicBinary = erlang:list_to_binary(Topic),
            lager:info("Subscribing to topic: ~s", [TopicBinary]),
            
            %% emqttc的subscribe可能直接返回ok或{ok, _}
            case emqttc:subscribe(Client, TopicBinary, qos1) of
                ok ->
                    lager:info("Subscribed to topic: ~s successfully", [TopicBinary]),
                    {ok, Client};
                {ok, _} ->
                    lager:info("Subscribed to topic: ~s successfully", [TopicBinary]),
                    {ok, Client};
                Error ->
                    lager:error("Subscribe failed: ~p", [Error]),
                    emqttc:disconnect(Client),
                    {error, subscribe_failed}
            end;
        {error, Reason} ->
            lager:error("Failed to start EMQX client: ~p", [Reason]),
            {error, Reason}
    end.
```

**4. 修改handle_info处理消息（第106-116行）：**
```erlang
%% emqttc的消息格式可能是 {publish, Topic, Payload}
handle_info({publish, Topic, Payload}, State) ->
    lager:info("Received MQTT message from: ~s", [Topic]),
    lager:info("Payload: ~p", [Payload]),
    case handle_device_data(Payload) of
        ok ->
            {noreply, State};
        {error, Reason} ->
            lager:error("Failed to handle device data: ~p", [Reason]),
            {noreply, State}
    end;
%% 处理断开连接
handle_info({mqttc, _Client, disconnected}, State) ->
    lager:warning("EMQX disconnected, will reconnect in 5s"),
    erlang:send_after(5000, self(), reconnect),
    {noreply, State#state{emqx_client = undefined}};
%% 处理重连
handle_info(reconnect, State) ->
    case connect_emqx(State) of
        {ok, Client} ->
            lager:info("EMQX reconnected"),
            {noreply, State#state{emqx_client = Client}};
        {error, _} ->
            erlang:send_after(5000, self(), reconnect),
            {noreply, State}
    end;
handle_info(Info, State) ->
    lager:debug("Unknown message: ~p", [Info]),
    {noreply, State}.
```

#### 方案2：使用supervisor管理（推荐）

确保emqx_sync_service在supervisor树中，这样如果连接失败会自动重启。检查 `src/eadm_sup.erl` 中是否正确配置。

### 测试步骤

1. 停止当前运行的应用
2. 修改代码
3. 重新编译：`rebar3 compile`
4. 启动：`rebar3 shell`
5. 观察日志，确认看到 "Subscribed to topic: XXX successfully"
6. 向MQTT主题发送测试消息，确认能收到并打印

### 调试建议

如果修改后还是不行，可以在shell中手动测试：

```erlang
%% 获取emqx_sync_service的状态
sys:get_state(emqx_sync_service).

%% 查看进程消息队列
process_info(whereis(emqx_sync_service), messages).

%% 手动发送测试消息
emqx_sync_service ! {publish, <<  "/test">>, <<"{\"test\":1}">>}.
```
