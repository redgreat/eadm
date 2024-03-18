%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2024, REDGREAT
%% @doc
%%
%% 我的财务逻辑处理
%%
%% @end
%%% Created : 2024-03-15 08:39:40
%%%-------------------------------------------------------------------
-module(eadm_finance_controller).
-author("wangcw").

%%%===================================================================
%%% Application callbacks
%%%===================================================================
-export([index/1, search/1, searchdetail/1, upload/1]).


%%====================================================================
%% API functions
%%====================================================================

%% @doc
%% index
%% @end
index(#{auth_data := #{<<"authed">> := true, <<"username">> := Username}}) ->
    {ok, [{username, Username}]};

index(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 查询返回数据结果
%% @end
search(#{auth_data := #{<<"authed">> := true},
    parsed_qs := #{<<"sourceType">> := SourceType, <<"inorOut">> := InOrOut,
        <<"startTime">> := StartTime, <<"endTime">> := EndTime}}) ->
    MaxSearchSpan = application:get_env(restwong_cfg, max_fin_search_span, 366),
    TimeDiff = eadm_utils:time_diff(StartTime, EndTime),
    case TimeDiff > (MaxSearchSpan * 86400) of
        true ->
            Alert = #{<<"Alert">> => unicode:characters_to_binary("查询时长超过 " ++ integer_to_list(MaxSearchSpan) ++ " 天，禁止查询!")},
            io:format("Alert: ~p~n", [Alert]),
            {json, [Alert]};
        _ ->
            try
                case {SourceType, InOrOut} of
                    {<<"0">>, <<"0">>} ->
                        {ok, Res_Col, Res_Data} = mysql_pool:query(pool_db,
                            "SELECT Id, `Source` AS SourceType, InOrOut, TradeType, Amount, TradeTime
                            FROM paybilldetail
                            WHERE TradeTime >= ?
                              AND TradeTime < ?
                            ORDER BY TradeTime;",
                            [StartTime, EndTime]),
                        Response = eadm_utils:return_as_json(Res_Col, Res_Data),
                        {json, Response};
                    {<<"0">>, _} ->
                        {ok, Res_Col, Res_Data} = mysql_pool:query(pool_db,
                            "SELECT Id, `Source` AS SourceType, InOrOut, TradeType, Amount, TradeTime
                            FROM paybilldetail
                            WHERE TradeTime >= ?
                              AND TradeTime < ?
                              AND InOrOut = ?
                            ORDER BY TradeTime;",
                            [StartTime, EndTime, InOrOut]),
                        Response = eadm_utils:return_as_json(Res_Col, Res_Data),
                        {json, Response};
                    {_, <<"0">>} ->
                        {ok, Res_Col, Res_Data} = mysql_pool:query(pool_db,
                            "SELECT Id, `Source` AS SourceType, InOrOut, TradeType, Amount, TradeTime
                            FROM paybilldetail
                            WHERE TradeTime >= ?
                              AND TradeTime < ?
                              AND `Source` = ?
                            ORDER BY TradeTime;",
                            [StartTime, EndTime, SourceType]),
                        Response = eadm_utils:return_as_json(Res_Col, Res_Data),
                        {json, Response};
                    {_, _} ->
                        {ok, Res_Col, Res_Data} = mysql_pool:query(pool_db,
                            "SELECT Id, `Source` AS SourceType, InOrOut, TradeType, Amount, TradeTime
                            FROM paybilldetail
                            WHERE TradeTime >= ?
                              AND TradeTime < ?
                              AND `Source` = ?
                              AND InOrOut = ?
                            ORDER BY TradeTime;",
                            [StartTime, EndTime, SourceType, InOrOut]),
                        Response = eadm_utils:return_as_json(Res_Col, Res_Data),
                        {json, Response}
                end
            catch
                _:Error ->
                    Alert = #{<<"Alert">> => unicode:characters_to_binary("查询失败! " ++ Error)},
                    {json, [Alert]}
            end
    end;

search(#{auth_data := #{<<"authed">> := false}}) ->
    Alert = #{<<"Alert">> => unicode:characters_to_binary("权限校验失败，请刷新页面重新登录! ")},
    {json, [Alert]}.

%% @doc
%% 查询返回数据明细
%% @end
searchdetail(#{auth_data := #{<<"authed">> := true},
    bindings := #{<<"detailId">> := DetailId}}) ->
    try
        Res_Data = mysql_pool:query(pool_db,
            "SELECT Owner, `Source` AS SourceType, InOrOut, CounterParty, Counterbank, CounterAccount,
               GoodsComment, PayMethod, Amount, Balance, Currency, PayStatus,
               TradeType, TradeOrderNo, CounterorderNo, TradeTime, BillComment
             FROM paybilldetail
             WHERE Id = ?;",
            [DetailId]),
        Response = eadm_utils:as_map(Res_Data),
        {json, Response}
    catch
        _:Error ->
            Alert = #{<<"Alert">> => unicode:characters_to_binary("查询失败! " ++ Error)},
            {json, [Alert]}
    end;

searchdetail(#{auth_data := #{<<"authed">> := false}}) ->
    Alert = #{<<"Alert">> => unicode:characters_to_binary("权限校验失败，请刷新页面重新登录! ")},
    {json, [Alert]}.

%% @doc
%% 处理上传数据
%% @end
% upload(#{auth_data := #{<<"authed">> := true},
%     bindings := #{<<"detailId">> := DetailId}}) ->
upload(Req) ->
    io:format("Req: ~p~n", [Req]),
    {ok, FileContent, Req2} = cowboy_req:read_body(Req),
    DataToInsert = #{
        'Owner' => <<"OwnerName">>,
        'SourceType' => <<"SourceTypeValue">>,
        'InOrOut' => <<"InOrOut">>,
        'CounterParty' => <<"CounterParty">>,
        'CounterBank' => <<"CounterBank">>,
        'CounterAccount' => <<"CounterAccount">>,
        'GoodsComment' => <<"GoodsComment">>,
        'PayMethod' => <<"PayMethod">>,
        'Amount' => <<"Amount">>,
        'Balance' => <<"Balance">>,
        'Currency' => <<"Currency">>,
        'PayStatus' => <<"PayStatus">>,
        'TradeType' => <<"TradeType">>,
        'TradeOrderNo' => <<"TradeOrderNo">>,
        'CounterOrderNo' => <<"CounterOrderNo">>,
        'TradeTime' => <<"TradeTime">>,
        'BillComment' => <<"BillComment">>
    },

    try
        Res_Data = mysql_pool:query(pool_db,
            "SELECT Owner, `Source` AS SourceType, InOrOut, CounterParty, Counterbank, CounterAccount,
               GoodsComment, PayMethod, Amount, Balance, Currency, PayStatus,
               TradeType, TradeOrderNo, CounterorderNo, TradeTime, BillComment
             FROM paybilldetail
             WHERE Id = ?;",
            [Req]),
        Response = eadm_utils:as_map(Res_Data),
        {json, Response}
    catch
        _:Error ->
            Alert = #{<<"Alert">> => unicode:characters_to_binary("查询失败! " ++ Error)},
            {json, [Alert]}
    end;

upload(#{auth_data := #{<<"authed">> := false}}) ->
    Alert = #{<<"Alert">> => unicode:characters_to_binary("权限校验失败，请刷新页面重新登录! ")},
    {json, [Alert]}.

%%====================================================================
%% Internal functions
%%====================================================================
