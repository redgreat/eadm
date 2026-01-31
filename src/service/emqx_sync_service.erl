%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2026, REDGREAT
%%% @doc
%%% EMQX消息接收服务
%%% 从EMQX实时接收设备数据并存储到PostgreSQL
%%% @end
%%% Created : 2026-01-28
%%%-------------------------------------------------------------------

-module(emqx_sync_service).

-behaviour(gen_server).

-export([start_link/0, stop/0]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(SERVER, ?MODULE).

-record(state, {
    emqx_host,
    emqx_port,
    emqx_username,
    emqx_password,
    emqx_client_id,
    emqx_topics,
    emqx_ssl,
    emqx_ssl_opts,
    emqx_proto_ver,
    emqx_connect_timeout,
    emqx_client
}).

%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc
%% 启动服务
%% @end
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc
%% 停止服务
%% @end
-spec stop() -> ok.
stop() ->
    gen_server:call(?SERVER, stop).

%% @private
%% @doc
%% 初始化服务
%% @end
init([]) ->
    process_flag(trap_exit, true),
    case get_emqx_env() of
        {ok, EmqxEnv} ->
            State = build_state(EmqxEnv),
            case connect_emqx(State) of
                {ok, EmqxClient} ->
                    lager:info(
                        "EMQX connected: client_id=~s",
                        [State#state.emqx_client_id]
                    ),
                    {ok, State#state{emqx_client = EmqxClient}};
                {error, Reason} ->
                    lager:error("Failed to connect to EMQX: ~p", [Reason]),
                    {stop, {failed_to_connect, Reason}}
            end;
        {error, Reason} ->
            lager:error("EMQX config not found: ~p", [Reason]),
            {ok, #state{emqx_client = undefined}}
    end.

%% @private
%% @doc
%% 处理同步停止请求
%% @end
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
%% @doc
%% 忽略未知同步调用
%% @end
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

%% @private
%% @doc
%% 忽略未知异步消息
%% @end
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
%% @doc
%% 处理 mqtt_message 消息格式
%% @end
handle_info({mqtt_message, Payload}, State) ->
    case handle_device_data(Payload) of
        ok ->
            {noreply, State};
        {error, Reason} ->
            lager:error("Failed to handle device data: ~p", [Reason]),
            {noreply, State}
    end;
%% @doc
%% 处理 publish 消息格式
%% @end
handle_info({publish, #{payload := Payload}}, State) ->
    case handle_device_data(Payload) of
        ok ->
            {noreply, State};
        {error, Reason} ->
            lager:error("Failed to handle device data: ~p", [Reason]),
            {noreply, State}
    end;
%% @doc
%% 忽略其他未知消息
%% @end
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
%% @doc
%% 终止时关闭 MQTT 客户端
%% @end
terminate(_Reason, #state{emqx_client = EmqxClient}) ->
    lager:info("EMQX sync service stopped"),
    case EmqxClient of
        undefined -> ok;
        _ -> emqtt:stop(EmqxClient)
    end,
    ok.

%% @private
%% @doc
%% 热升级回调
%% @end
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
%% @doc
%% 使用pgpool插入设备数据到PostgreSQL
%% @end
insert_device_data(
    Imei,
    Imsi,
    Lat,
    Lng,
    AgpsLat,
    AgpsLng,
    Uptime,
    Rsrp,
    Csq,
    Vbat,
    AgpsTs,
    GpsTs,
    Rssi,
    Rsrq,
    Snr
) ->
    %% 构造SQL插入语句
    Sql =
        "INSERT INTO emqx_device_data (imei, imsi, lat, lng, agps_lat, agps_lng, " ++
            "uptime, rsrp, csq, vbat, agps_ts, gps_ts, rssi, rsrq, snr, receivetime) " ++
            "VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15, NOW())",

    %% 参数列表
    Params = [
        Imei,
        Imsi,
        Lat,
        Lng,
        AgpsLat,
        AgpsLng,
        Uptime,
        Rsrp,
        Csq,
        Vbat,
        AgpsTs,
        GpsTs,
        Rssi,
        Rsrq,
        Snr
    ],

    %% 使用pgpool执行SQL
    case eadm_pgpool:equery(pool_pg, Sql, Params) of
        {ok, _Result} ->
            ok;
        {error, Reason} ->
            lager:error("Failed to insert device data to PostgreSQL: ~p", [Reason]),
            {error, Reason}
    end.

%% @private
%% @doc
%% 连接EMQX服务器
%% @end
connect_emqx(#state{
    emqx_host = Host,
    emqx_port = Port,
    emqx_username = Username,
    emqx_password = Password,
    emqx_client_id = ClientId,
    emqx_topics = Topics,
    emqx_ssl = SslEnabled,
    emqx_ssl_opts = SslOpts,
    emqx_proto_ver = ProtoVer,
    emqx_connect_timeout = ConnectTimeout
}) ->
    BaseOptions = [
        {host, Host},
        {port, Port},
        {username, Username},
        {password, Password},
        {clientid, ClientId},
        {clean_start, true},
        {keepalive, 60},
        {connect_timeout, ConnectTimeout}
    ],
    OptionsWithSsl =
        case SslEnabled =:= true orelse SslOpts =/= [] of
            true ->
                BaseOptions ++ [{ssl, true}, {ssl_opts, SslOpts}];
            false ->
                BaseOptions
        end,
    Options =
        case ProtoVer of
            undefined -> OptionsWithSsl;
            _ -> OptionsWithSsl ++ [{proto_ver, ProtoVer}]
        end,
    case catch emqtt:start_link(Options) of
        {ok, Client} ->
            ConnectResult = catch emqtt:connect(Client),
            case ConnectResult of
                {ok, _Props} ->
                    subscribe_topics(Client, Topics);
                {error, Reason} ->
                    {error, Reason};
                {'EXIT', Reason} ->
                    {error, Reason};
                Other ->
                    {error, Other}
            end;
        {error, Reason} ->
            {error, Reason};
        {'EXIT', Reason} ->
            {error, Reason}
    end.

%% @private
%% @doc
%% 处理设备数据
%% @end
handle_device_data(Payload) ->
    try
        Data = json:decode(Payload),

        %% 提取需要的字段
        Imei = maps:get(<<"imei">>, Data, <<>>),
        Imsi = maps:get(<<"imsi">>, Data, <<>>),
        Lat = maps:get(<<"lat">>, Data, 0.0),
        Lng = maps:get(<<"lng">>, Data, 0.0),
        AgpsLat = maps:get(<<"agps_lat">>, Data, 0.0),
        AgpsLng = maps:get(<<"agps_lng">>, Data, 0.0),
        Uptime = maps:get(<<"uptime">>, Data, 0),
        Rsrp = maps:get(<<"rsrp">>, Data, 0),
        Csq = maps:get(<<"csq">>, Data, 0),
        Vbat = maps:get(<<"vbat">>, Data, 0),
        AgpsTs = maps:get(<<"agps_ts">>, Data, 0),
        GpsTs = maps:get(<<"gps_ts">>, Data, 0),
        Rssi = maps:get(<<"rssi">>, Data, 0),
        Rsrq = maps:get(<<"rsrq">>, Data, 0),
        Snr = maps:get(<<"snr">>, Data, 0),

        %% 插入PostgreSQL数据库
        case
            insert_device_data(
                Imei,
                Imsi,
                Lat,
                Lng,
                AgpsLat,
                AgpsLng,
                Uptime,
                Rsrp,
                Csq,
                Vbat,
                AgpsTs,
                GpsTs,
                Rssi,
                Rsrq,
                Snr
            )
        of
            ok ->
                ok;
            {error, InsertReason} ->
                lager:error("Failed to insert device data for IMEI ~s: ~p", [Imei, InsertReason]),
                {error, InsertReason}
        end
    catch
        error:ProcessReason ->
            lager:error("Failed to process device data: ~p, Payload: ~s", [ProcessReason, Payload]),
            {error, ProcessReason}
    end.

%% @private
%% @doc
%% 解析并生成客户端ID
%% @end
resolve_client_id(ClientId, Prefix) ->
    case ClientId of
        undefined ->
            iolist_to_binary([Prefix, integer_to_binary(erlang:system_time())]);
        Id when is_binary(Id) ->
            Id;
        Id when is_list(Id) ->
            list_to_binary(Id);
        Id ->
            iolist_to_binary(Id)
    end.

%% @private
%% @doc
%% 规范化单个主题为二进制
%% @end
normalize_topic(undefined) ->
    undefined;
normalize_topic(Topic) when is_binary(Topic) ->
    Topic;
normalize_topic(Topic) when is_list(Topic) ->
    list_to_binary(Topic);
normalize_topic(Topic) ->
    iolist_to_binary(Topic).

%% @private
%% @doc
%% 规范化主题列表为二进制列表
%% @end
normalize_topics(undefined) ->
    [];
normalize_topics(Topics) when is_list(Topics) ->
    case is_string_topic(Topics) of
        true -> [list_to_binary(Topics)];
        false -> [normalize_topic(Topic) || Topic <- Topics]
    end;
normalize_topics(Topic) ->
    [normalize_topic(Topic)].

%% @private
%% @doc
%% 判断是否为单个字符串主题
%% @end
is_string_topic(Topic) ->
    is_list(Topic) andalso lists:all(fun is_integer/1, Topic).

%% @private
%% @doc
%% 获取 EMQX 配置（优先 eadm 应用环境）
%% @end
get_emqx_env() ->
    case application:get_env(eadm, emqx_host) of
        {ok, EmqxHost} ->
            {ok, EmqxPort} = application:get_env(eadm, emqx_port),
            {ok, EmqxUsername} = application:get_env(eadm, emqx_username),
            {ok, EmqxPassword} = application:get_env(eadm, emqx_password),
            {ok, EmqxClientId} = application:get_env(eadm, emqx_client_id),
            {ok, EmqxTopics} = application:get_env(eadm, emqx_topics),
            EmqxSsl = get_env_default(eadm, emqx_ssl, false),
            EmqxSslOpts = get_env_default(eadm, emqx_ssl_opts, []),
            EmqxProtoVer = get_env_default(eadm, emqx_proto_ver, undefined),
            EmqxConnectTimeout = get_env_default(eadm, emqx_connect_timeout, 10),
            {ok, #{
                host => EmqxHost,
                port => EmqxPort,
                username => EmqxUsername,
                password => EmqxPassword,
                client_id => EmqxClientId,
                topics => EmqxTopics,
                ssl => EmqxSsl,
                ssl_opts => EmqxSslOpts,
                proto_ver => EmqxProtoVer,
                connect_timeout => EmqxConnectTimeout
            }};
        _ ->
            case application:get_env(emqx, pools) of
                {ok, EmqxPools} when is_list(EmqxPools), EmqxPools =/= [] ->
                    {pool_emqx, _EmqxPoolOpts, EmqxConnOpts} = hd(EmqxPools),
                    EmqxHost = proplists:get_value(host, EmqxConnOpts),
                    EmqxPort = proplists:get_value(port, EmqxConnOpts),
                    EmqxUsername = proplists:get_value(username, EmqxConnOpts),
                    EmqxPassword = proplists:get_value(password, EmqxConnOpts),
                    EmqxClientId =
                        resolve_client_id(
                            proplists:get_value(client_id, EmqxConnOpts, undefined),
                            proplists:get_value(client_id_prefix, EmqxConnOpts, "eadm_")
                        ),
                    EmqxTopics =
                        normalize_topics(
                            proplists:get_value(
                                topics,
                                EmqxConnOpts,
                                proplists:get_value(topic, EmqxConnOpts)
                            )
                        ),
                    EmqxSsl = proplists:get_value(ssl, EmqxConnOpts, false),
                    EmqxSslOpts = proplists:get_value(ssl_opts, EmqxConnOpts, []),
                    EmqxProtoVer = proplists:get_value(proto_ver, EmqxConnOpts, undefined),
                    EmqxConnectTimeout =
                        proplists:get_value(connect_timeout, EmqxConnOpts, 10),
                    {ok, #{
                        host => EmqxHost,
                        port => EmqxPort,
                        username => EmqxUsername,
                        password => EmqxPassword,
                        client_id => EmqxClientId,
                        topics => EmqxTopics,
                        ssl => EmqxSsl,
                        ssl_opts => EmqxSslOpts,
                        proto_ver => EmqxProtoVer,
                        connect_timeout => EmqxConnectTimeout
                    }};
                Other ->
                    {error, Other}
            end
    end.

%% @private
%% @doc
%% 读取应用环境默认值
%% @end
get_env_default(App, Key, Default) ->
    case application:get_env(App, Key) of
        {ok, Value} -> Value;
        _ -> Default
    end.

%% @private
%% @doc
%% 构建内部状态记录
%% @end
build_state(EmqxEnv) ->
    #state{
        emqx_host = maps:get(host, EmqxEnv),
        emqx_port = maps:get(port, EmqxEnv),
        emqx_username = maps:get(username, EmqxEnv),
        emqx_password = maps:get(password, EmqxEnv),
        emqx_client_id = maps:get(client_id, EmqxEnv),
        emqx_topics = normalize_topics(maps:get(topics, EmqxEnv, [])),
        emqx_ssl = maps:get(ssl, EmqxEnv, false),
        emqx_ssl_opts = maps:get(ssl_opts, EmqxEnv, []),
        emqx_proto_ver = maps:get(proto_ver, EmqxEnv, undefined),
        emqx_connect_timeout = maps:get(connect_timeout, EmqxEnv, 10)
    }.

%% @private
%% @doc
%% 订阅主题列表
%% @end
subscribe_topics(Client, Topics) ->
    lists:foreach(
        fun(Topic) ->
            case emqtt:subscribe(Client, Topic, 0) of
                {ok, _Props, _ReasonCodes} ->
                    lager:info("Subscribed to topic: ~p", [Topic]);
                {ok, _ReasonCodes} ->
                    lager:info("Subscribed to topic: ~p", [Topic]);
                ok ->
                    lager:info("Subscribed to topic: ~p", [Topic]);
                {error, Reason} ->
                    lager:error("Failed to subscribe to topic ~p: ~p", [Topic, Reason])
            end
        end,
        Topics
    ),
    {ok, Client}.
