%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2024, REDGREAT
%%% @doc
%%%
%%% 我的财务逻辑处理
%%%
%%% @end
%%% Created : 2024-03-15 08:39:40
%%%-------------------------------------------------------------------
-module(eadm_finance_controller).
-author("wangcw").

%%%===================================================================
%%% 函数导出
%%%===================================================================
-export([index/1, search/1, delete/1, searchdetail/1, upload/1]).


%%====================================================================
%% API 函数
%%====================================================================

%% @doc
%% 主函数
%% @end
index(#{auth_data := #{<<"authed">> := true, <<"username">> := UserName,
      <<"permission">> := #{<<"finance">> := #{<<"finlist">> := true}}}}) ->
    {ok, [{username, UserName}]};

index(#{auth_data := #{<<"permission">> := #{<<"finance">> := #{<<"finlist">> := false}}}}) ->
    {json, [#{<<"Alert">> => unicode:characters_to_binary("API鉴权失败！", utf8)}]};

index(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 查询返回数据结果
%% @end
search(#{auth_data := #{<<"authed">> := true,
      <<"permission">> := #{<<"finance">> := #{<<"finlist">> := true}}},
      parsed_qs := #{<<"sourceType">> := SourceType, <<"inorOut">> := InOrOut,
        <<"startTime">> := StartTime, <<"endTime">> := EndTime}}) ->
    case {eadm_utils:validate_date_time(StartTime), eadm_utils:validate_date_time(EndTime)} of
        {false, _} ->
            {json, [#{<<"Alert">> => unicode:characters_to_binary("开始时间格式错误！", utf8)}]};
        {_, false} ->
            {json, [#{<<"Alert">> => unicode:characters_to_binary("结束时间格式错误！", utf8)}]};
        {_, _} ->
            ParameterStartTime = eadm_utils:parse_date_time(StartTime),
            ParameterEndTime = eadm_utils:parse_date_time(EndTime),
            ParameterSourceType = erlang:binary_to_integer(SourceType),
            ParameterInOrOut = case InOrOut of 1 -> <<"收入">>; 2 -> <<"支出">>; 3 -> <<"其他">>; _ -> 0 end,
            MaxSearchSpan = application:get_env(restwong_cfg, max_fin_search_span, 366),
            TimeDiff = eadm_utils:time_diff(StartTime, EndTime),
            case TimeDiff > (MaxSearchSpan * 86400) of
                true ->
                    {json, [#{<<"Alert">> => unicode:characters_to_binary(("查询时长超过 "
                        ++ erlang:integer_to_list(MaxSearchSpan) ++ " 天，禁止查询!"), utf8)}]};
                _ ->
                    try
                        case {ParameterSourceType, ParameterInOrOut} of
                            {0, 0} ->
                                {ok, ResCol, ResData} = eadm_pgpool:equery(pool_pg,
                                    "select id, sourcetype, inorout, tradetype, amount, tradetime
                                    from fn_paybilldetail
                                    where tradetime >= $1
                                      and tradetime < $2
                                      and deleted is false
                                    order by tradetime;",
                                    [ParameterStartTime, ParameterEndTime]),
                                {json, eadm_utils:pg_as_json(ResCol, ResData)};
                            {0, _} ->
                                {ok, ResCol, ResData} = eadm_pgpool:equery(pool_pg,
                                    "select id, sourcetype, inorout, tradetype, amount, tradetime
                                    from fn_paybilldetail
                                    where tradetime >= $1
                                      and tradetime < $2
                                      and inorout = $3
                                      and deleted is false
                                    order by tradetime;",
                                    [ParameterStartTime, ParameterEndTime, ParameterInOrOut]),
                                {json, eadm_utils:pg_as_json(ResCol, ResData)};
                            {_, 0} ->
                                {ok, ResCol, ResData} = eadm_pgpool:equery(pool_pg,
                                    "select id, sourcetype, inorout, tradetype, amount, tradetime
                                    from fn_paybilldetail
                                    where tradetime >= $1
                                      and tradetime < $2
                                      and sourcetype = $3
                                      and deleted is false
                                    order by tradetime;",
                                    [ParameterStartTime, ParameterEndTime, ParameterSourceType]),
                                {json, eadm_utils:pg_as_json(ResCol, ResData)};
                            _ ->
                                {ok, ResCol, ResData} = eadm_pgpool:equery(pool_pg,
                                    "select id, sourcetype, inorout, tradetype, amount, tradetime
                                    from fn_paybilldetail
                                    where tradetime >= $1
                                      and tradetime < $2
                                      and sourcetype = $3
                                      and inorout = $4
                                      and deleted is false
                                    order by tradetime;",
                                    [ParameterStartTime, ParameterEndTime, ParameterSourceType, ParameterInOrOut]),
                                {json, eadm_utils:pg_as_json(ResCol, ResData)}
                        end
                    catch
                        _:Error ->
                            lager:error("数据查询失败: ~p~n", [Error]),
                            {json, [#{<<"Alert">> => unicode:characters_to_binary("数据查询失败！", utf8)}]}
                    end
            end
    end;

search(#{auth_data := #{<<"permission">> := #{<<"finance">> := #{<<"finlist">> := false}}}}) ->
    {json, [#{<<"Alert">> => unicode:characters_to_binary("API鉴权失败！", utf8)}]};

search(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 删除财务数据
%% @end
delete(#{auth_data := #{<<"authed">> := true, <<"loginname">> := LoginName,
      <<"permission">> := #{<<"finance">> := #{<<"findel">> := true}}},
    bindings := #{<<"detailId">> := DetailId}}) ->
    ParameterDetailId = erlang:binary_to_integer(DetailId),
        try
            eadm_pgpool:equery(pool_pg, "update fn_paybilldetail
                                      set deleteduser = $1,
                                      deletedat = current_timestamp,
                                      deleted = true
                                      where id = $2;",
                                      [LoginName, ParameterDetailId]),
            {json, [#{<<"Alert">> => unicode:characters_to_binary("数据删成功！", utf8)}]}
        catch
            _:Error ->
                lager:error("数据删除失败: ~p~n", [Error]),
                {json, [#{<<"Alert">> => unicode:characters_to_binary("数据删除失败！", utf8)}]}
        end;

delete(#{auth_data := #{<<"permission">> := #{<<"finance">> := #{<<"findel">> := false}}}}) ->
    {json, [#{<<"Alert">> => unicode:characters_to_binary("API鉴权失败！", utf8)}]};

delete(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 查询返回数据明细
%% @end
searchdetail(#{auth_data := #{<<"authed">> := true,
      <<"permission">> := #{<<"finance">> := #{<<"finlist">> := true}}},
    bindings := #{<<"detailId">> := DetailId}}) ->
    try
        {ok, ResCol, ResData} = eadm_pgpool:equery(pool_pg,
            "select owner, sourcetype, inorout, counterparty, counterbank, counteraccount,
               goodscomment, paymethod, amount, balance, currency, paystatus,
               tradetype, tradeorderno, counterorderno, tradetime, billcomment
             from fn_paybilldetail
             where deleted is false
               and id = $1;",
            [erlang:binary_to_integer(DetailId)]),
        {json, eadm_utils:pg_as_map(ResCol, ResData)}
    catch
        _:Error ->
            lager:error("用户信息查询失败: ~p~n", [Error]),
            {json, [#{<<"Alert">> => unicode:characters_to_binary("用户信息查询失败！", utf8)}]}
    end;

searchdetail(#{auth_data := #{<<"permission">> := #{<<"finance">> := #{<<"finlist">> := false}}}}) ->
    {json, [#{<<"Alert">> => unicode:characters_to_binary("API鉴权失败！", utf8)}]};

searchdetail(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 处理上传数据
%% @end
upload(#{auth_data := #{<<"authed">> := true,
      <<"permission">> := #{<<"finance">> := #{<<"finlist">> := true}}},
      json := #{<<"importType">> := ImportType, <<"uploadJson">> := UploadJson}}) ->
    case ImportType of
        <<"0">> ->
            lists:foreach(
                fun(Map) ->
                    try
                        eadm_pgpool:equery(pool_pg,
                            "insert into fn_paybilldetail(owner, sourcetype, inorout, counterparty, counterbank,
                             counteraccount, goodscomment, paymethod, amount, balance, currency, paystatus,
                             tradetype, tradeorderno, counterorderno, tradetime, billcomment)
                            values ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15, $16, $17);",
                            [maps:get(<<"Owner">>, Map, null),
                             maps:get(<<"Source">>, Map, null),
                             maps:get(<<"InOrOut">>, Map, null),
                             maps:get(<<"CounterParty">>, Map, null),
                             maps:get(<<"CounterBank">>, Map, null),
                             maps:get(<<"CounterAccount">>, Map, null),
                             maps:get(<<"GoodsComment">>, Map, null),
                             maps:get(<<"PayMethod">>, Map, null),
                             maps:get(<<"Amount">>, Map, null),
                             maps:get(<<"Balance">>, Map, null),
                             maps:get(<<"Currency">>, Map, null),
                             maps:get(<<"PayStatus">>, Map, null),
                             maps:get(<<"TradeType">>, Map, null),
                             maps:get(<<"TradeOrderNo">>, Map, null),
                             maps:get(<<"CounterorderNo">>, Map, null),
                             maps:get(<<"TradeTime">>, Map, null),
                             maps:get(<<"BillComment">>, Map, null)]
                        )
                    catch
                        _:Error ->
                            lager:error("数据插入失败：~p~n", [Error]),
                            {json, [#{<<"Alert">> => unicode:characters_to_binary("数据插入失败！", utf8)}]}
                    end
                end,
                UploadJson),
            {json, [#{<<"Alert">> => unicode:characters_to_binary(("导入成功"
                ++ erlang:integer_to_list(count_maps(UploadJson)) ++ "行！"), utf8)}]};
        <<"1">> ->
            lists:foreach(
                fun(Map) ->
                    try
                        eadm_pgpool:equery(pool_pg,
                            "insert into fn_paybilldetail(owner, sourcetype, tradetime, tradetype, counterparty, goodscomment,
                            inorout, amount, paymethod, paystatus, tradeorderno, counterorderno, billcomment)
                            values ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13);",
                            [maps:get(<<"Owner">>, Map, null),
                             maps:get(<<"Source">>, Map, null),
                             maps:get(<<"TradeTime">>, Map, null),
                             maps:get(<<"TradeType">>, Map, null),
                             maps:get(<<"CounterParty">>, Map, null),
                             maps:get(<<"GoodsComment">>, Map, null),
                             maps:get(<<"InOrOut">>, Map, null),
                             maps:get(<<"Amount">>, Map, null),
                             maps:get(<<"PayMethod">>, Map, null),
                             maps:get(<<"PayStatus">>, Map, null),
                             maps:get(<<"TradeOrderNo">>, Map, null),
                             maps:get(<<"CounterOrderNo">>, Map, null),
                             maps:get(<<"BillComment">>, Map, null)]
                        )
                    catch
                        _:Error ->
                            lager:error("数据插入失败：~p~n", [Error]),
                            {json, [#{<<"Alert">> => unicode:characters_to_binary("数据插入失败！", utf8)}]}
                    end
                end,
                UploadJson),
            {json, [#{<<"Alert">> => unicode:characters_to_binary(("导入成功"
                ++ erlang:integer_to_list(count_maps(UploadJson)) ++ "行！"), utf8)}]};
        <<"2">> ->
            lists:foreach(
                fun(Map) ->
                    try
                        eadm_pgpool:equery(pool_pg,
                            "insert into fn_paybilldetail(owner, sourcetype, tradeorderno, counterorderno, tradetime,
                            paymethod, counterparty, goodscomment, amount, inorout, paystatus, billcomment)
                            values ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12);",
                            [maps:get(<<"Owner">>, Map, null),
                             maps:get(<<"Source">>, Map, null),
                             maps:get(<<"TradeOrderNo">>, Map, null),
                             maps:get(<<"CounterOrderNo">>, Map, null),
                             maps:get(<<"TradeTime">>, Map, null),
                             maps:get(<<"PayMethod">>, Map, null),
                             maps:get(<<"CounterParty">>, Map, null),
                             maps:get(<<"GoodsComment">>, Map, null),
                             maps:get(<<"Amount">>, Map, null),
                             maps:get(<<"InOrOut">>, Map, null),
                             maps:get(<<"PayStatus">>, Map, null),
                             maps:get(<<"BillComment">>, Map, null)]
                        )
                    catch
                        _:Error ->
                            lager:error("数据插入失败：~p~n", [Error]),
                            {json, [#{<<"Alert">> => unicode:characters_to_binary("数据插入失败！", utf8)}]}
                    end
                end,
                UploadJson),
            {json, [#{<<"Alert">> => unicode:characters_to_binary(("导入成功"
                ++ erlang:integer_to_list(count_maps(UploadJson)) ++ "行！"), utf8)}]};
        <<"3">> ->
            lists:foreach(
                fun(Map) ->
                    InCome = maps:get(<<"Amount">>, Map),
                    case InCome of
                        _ when InCome > 0 ->
                        InOrOut = unicode:characters_to_binary("收入", uft8);
                        _ ->
                        InOrOut = unicode:characters_to_binary("支出", utf8)
                    end,
                    try
                        eadm_pgpool:equery(pool_pg,
                            "insert into fn_paybilldetail(owner, sourcetype, tradetime, counterparty,
                            counterbank, counteraccount, goodscomment, amount, balance, inorout)
                            values ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10);",
                            [maps:get(<<"Owner">>, Map, null),
                             maps:get(<<"Source">>, Map, null),
                             maps:get(<<"TradeTime">>, Map, null),
                             maps:get(<<"CounterParty">>, Map, null),
                             maps:get(<<"CounterBank">>, Map, null),
                             maps:get(<<"CounterAccount">>, Map, null),
                             maps:get(<<"GoodsComment">>, Map, null),
                             InCome,
                             maps:get(<<"Balance">>, Map, null),
                             InOrOut]
                        )
                    catch
                        _:Error ->
                            lager:error("数据插入失败：~p~n", [Error]),
                            {json, [#{<<"Alert">> => unicode:characters_to_binary("数据插入失败！", utf8)}]}
                    end
                end,
                UploadJson),
            {json, [#{<<"Alert">> => unicode:characters_to_binary(("导入成功"
                ++ erlang:integer_to_list(count_maps(UploadJson)) ++ "行！"), utf8)}]};
        <<"4">> ->
            {json, [#{<<"Alert">> => unicode:characters_to_binary(("导入成功"
                ++ erlang:integer_to_list(count_maps(UploadJson)) ++ "行！"), utf8)}]};
        _ ->
            {json, [unicode:characters_to_binary("导入格式不对！", utf8)]}
    end;

upload(#{auth_data := #{<<"permission">> := #{<<"finance">> := #{<<"finimp">> := false}}}}) ->
    {json, [#{<<"Alert">> => unicode:characters_to_binary("API鉴权失败！", utf8)}]};

upload(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%%====================================================================
%% 内部函数
%%====================================================================
count_maps(List) ->
    lists:foldl(fun(Elem, Acc) ->
        case is_map(Elem) of
            true -> Acc + 1;
            false -> Acc
        end
    end, 0, List).
