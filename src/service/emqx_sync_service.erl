%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2026, REDGREAT
%%% @doc
%%% EMQX消息接收服务
%%% 从EMQX实时接收设备数据并存储到TDengine（HTTP方式）
%%% @end
%%% Created : 2026-01-28
%%%-------------------------------------------------------------------

-module(emqx_sync_service).

-behaviour(gen_server).

%% API
-export([start_link/0, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
    emqx_host,
    emqx_port,
    emqx_username,
    emqx_password,
    emqx_client_id,
    emqx_client,
    td_host,
    td_port,
    td_username,
    td_password,
    td_database,
    td_token
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

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc
%% 初始化服务
%% @end
init([]) ->
    lager:info("Starting EMQX sync service..."),
    
    %% 读取配置
    {ok, EmqxConfig} = application:get_env(eadm, emqx),
    {ok, TdConfig} = application:get_env(eadm, tdengine),
    
    #{
        host := EmqxHost,
        port := EmqxPort,
        username := EmqxUsername,
        password := EmqxPassword
    } = EmqxConfig,
    
    #{
        host := TdHost,
        port := TdPort,
        username := TdUsername,
        password := TdPassword,
        database := TdDatabase
    } = TdConfig,
    
    EmqxClientId = <<"eadm_emqx_client_", (integer_to_binary(erlang:system_time()))/binary>>,
    
    State = #state{
        emqx_host = EmqxHost,
        emqx_port = EmqxPort,
        emqx_username = EmqxUsername,
        emqx_password = EmqxPassword,
        emqx_client_id = EmqxClientId,
        td_host = TdHost,
        td_port = TdPort,
        td_username = TdUsername,
        td_password = TdPassword,
        td_database = TdDatabase
    },
    
    %% 获取TDengine认证token
    case get_tdengine_token(State) of
        {ok, TdToken} ->
            lager:info("Got TDengine auth token successfully"),
            NewState = State#state{td_token = TdToken},
            
            %% 连接EMQX
            case connect_emqx(NewState) of
                {ok, EmqxClient} ->
                    lager:info("Connected to EMQX successfully"),
                    {ok, NewState#state{emqx_client = EmqxClient}};
                {error, Reason} ->
                    lager:error("Failed to connect to EMQX: ~p", [Reason]),
                    {stop, Reason}
            end;
        {error, Reason} ->
            lager:error("Failed to get TDengine token: ~p", [Reason]),
            {stop, Reason}
    end.

%% @private
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info({mqtt_message, Topic, Payload}, State) ->
    lager:info("Received message from topic: ~s", [Topic]),
    case handle_device_data(Payload, State) of
        ok ->
            {noreply, State};
        {error, Reason} ->
            lager:error("Failed to handle device data: ~p", [Reason]),
            {noreply, State}
    end;
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, #state{emqx_client = EmqxClient}) ->
    lager:info("EMQX sync service stopped"),
    case EmqxClient of
        undefined -> ok;
        _ -> emqttc:disconnect(EmqxClient)
    end,
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
%% @doc
%% 获取TDengine认证token
%% @end
get_tdengine_token(#state{
    td_host = Host,
    td_port = Port,
    td_username = Username,
    td_password = Password
}) ->
    %% 构造TDengine RESTful API URL
    Url = iolist_to_binary([
        "http://", Host, ":", integer_to_binary(Port), "/rest/login/"
    ]),
    
    %% 构造认证请求体
    AuthBody = jsx:encode(#{
        <<"username">> => Username,
        <<"password">> => Password
    }),
    
    %% 发送HTTP请求
    case httpc:request(post, {
        Url, 
        [{"Content-Type", "application/json"}], 
        "application/json", 
        AuthBody
    }, [], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(ResponseBody, [return_maps]) of
                #{<<"status">> := <<"succ">>, <<"code">> := 0, <<"desc">> := Token} ->
                    {ok, Token};
                Response ->
                    lager:error("TDengine auth response: ~p", [Response]),
                    {error, invalid_response}
            end;
        {error, Reason} ->
            lager:error("TDengine auth request failed: ~p", [Reason]),
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
    emqx_client_id = ClientId
}) ->
    Options = [
        {host, Host},
        {port, Port},
        {username, Username},
        {password, Password},
        {clientid, ClientId},
        {clean_start, true},
        {keepalive, 60}
    ],
    
    case emqttc:start_link(Options) of
        {ok, Client} ->
            %% 订阅设备数据主题
            case emqttc:subscribe(Client, <<"devices/+/data">>, 1) of
                {ok, _Props, _ReasonCodes} ->
                    lager:info("Subscribed to device data topic"),
                    {ok, Client};
                {error, Reason} ->
                    lager:error("Failed to subscribe to topic: ~p", [Reason]),
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @private
%% @doc
%% 处理设备数据
%% @end
handle_device_data(Payload, #state{td_token = Token} = State) ->
    try
        Data = jsx:decode(Payload, [return_maps]),
        
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
        
        %% 插入TDengine超级表
        insert_device_data(State, Token, Imei, Imsi, Lat, Lng, AgpsLat, AgpsLng, 
                          Uptime, Rsrp, Csq, Vbat, AgpsTs, GpsTs, Rssi, Rsrq, Snr),
        
        lager:info("Device data processed for IMEI: ~s", [Imei]),
        ok
    catch
        error:Reason ->
            lager:error("Failed to process device data: ~p, Payload: ~s", [Reason, Payload]),
            {error, Reason}
    end.

%% @private
%% @doc
%% 使用HTTP方式插入设备数据到TDengine
%% @end
insert_device_data(#state{
    td_host = Host,
    td_port = Port,
    td_database = Database
}, Token, Imei, Imsi, Lat, Lng, AgpsLat, AgpsLng, 
 Uptime, Rsrp, Csq, Vbat, AgpsTs, GpsTs, Rssi, Rsrq, Snr) ->
    %% 构造TDengine RESTful API URL
    Url = iolist_to_binary([
        "http://", Host, ":", integer_to_binary(Port), "/rest/sql/", Database
    ]),
    
    %% 构造SQL插入语句
    Sql = io_lib:format(
        "INSERT INTO device_data USING device_data TAGS('~s', '~s') VALUES "
        "(NOW, ~f, ~f, ~f, ~f, ~p, ~p, ~p, ~p, ~p, ~p, ~p, ~p, ~p)",
        [Imei, Imsi, Lat, Lng, AgpsLat, AgpsLng, Uptime, Rsrp, Csq, Vbat, 
         AgpsTs, GpsTs, Rssi, Rsrq, Snr]
    ),
    
    SqlBin = iolist_to_binary(Sql),
    
    %% 发送HTTP请求
    Headers = [
        {"Content-Type", "application/json"},
        {"Authorization", "Basic " ++ binary_to_list(base64:encode(Token))}
    ],
    
    Body = jsx:encode(#{
        <<"sql">> => SqlBin
    }),
    
    case httpc:request(post, {
        Url, Headers, "application/json", Body
    }, [], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(ResponseBody, [return_maps]) of
                #{<<"status">> := <<"succ">>, <<"code">> := 0} ->
                    ok;
                Response ->
                    lager:error("TDengine insert response: ~p", [Response]),
                    {error, insert_failed}
            end;
        {error, Reason} ->
            lager:error("TDengine insert request failed: ~p", [Reason]),
            {error, Reason}
    end.
