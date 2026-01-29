%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2026, REDGREAT
%%% @doc
%%% EMQX消息接收服务
%%% 从EMQX实时接收设备数据并存储到PostgreSQL（使用pgpool连接池）
%%% @end
%%% Created : 2026-01-28
%%%-------------------------------------------------------------------

-module(emqx_sync_service).

-behaviour(gen_server).

%% API
-export([start_link/0, stop/0]).

%% gen_server callbacks
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
    emqx_topic,
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

    #{
        host := EmqxHost,
        port := EmqxPort,
        username := EmqxUsername,
        password := EmqxPassword,
        topic := EmqxTopic
    } = EmqxConfig,

    EmqxClientId = <<"eadm_emqx_client_", (integer_to_binary(erlang:system_time()))/binary>>,

    State = #state{
        emqx_host = EmqxHost,
        emqx_port = EmqxPort,
        emqx_username = EmqxUsername,
        emqx_password = EmqxPassword,
        emqx_topic = EmqxTopic,
        emqx_client_id = EmqxClientId
    },

    %% 连接EMQX
    case connect_emqx(State) of
        {ok, EmqxClient} ->
            lager:info("Connected to EMQX successfully"),
            {ok, State#state{emqx_client = EmqxClient}};
        {error, Reason} ->
            lager:error("Failed to connect to EMQX: ~p", [Reason]),
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
    case handle_device_data(Payload) of
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
    case pgpool:query(Sql, Params) of
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
    emqx_topic = Topic
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
            TopicBinary = erlang:list_to_binary(Topic),
            lager:info("Subscribing to topic: ~s", [TopicBinary]),
            case emqttc:subscribe(Client, TopicBinary, 1) of
                {ok, _Props, _ReasonCodes} ->
                    lager:info("Subscribed to topic: ~s successfully", [TopicBinary]),
                    {ok, Client};
                {error, Reason} ->
                    lager:error("Failed to subscribe to topic ~s: ~p", [TopicBinary, Reason]),
                    {error, Reason}
            end;
        {error, Reason} ->
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
        ),

        lager:info("Device data processed for IMEI: ~s", [Imei]),
        ok
    catch
        error:Reason ->
            lager:error("Failed to process device data: ~p, Payload: ~s", [Reason, Payload]),
            {error, Reason}
    end.
