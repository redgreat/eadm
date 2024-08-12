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
%% index
%% @end
index(#{auth_data := #{<<"authed">> := true, <<"username">> := UserName,
      <<"permission">> := #{<<"finance">> := #{<<"finlist">> := true}}}}) ->
    {ok, [{username, UserName}]};

index(#{auth_data := #{<<"permission">> := #{<<"finance">> := #{<<"finlist">> := false}}}}) ->
    Alert = #{<<"Alert">> => unicode:characters_to_binary("API鉴权失败!")},
    {json, [Alert]};

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
            Alert = #{<<"Alert">> => unicode:characters_to_binary("开始时间格式错误！")},
            {json, [Alert]};
        {_, false} ->
            Alert = #{<<"Alert">> => unicode:characters_to_binary("结束时间格式错误！")},
            {json, [Alert]};
        {_, _} ->
            ParameterStartTime = eadm_utils:parse_date_time(StartTime),
            ParameterEndTime = eadm_utils:parse_date_time(EndTime),
            ParameterSourceType = binary_to_integer(SourceType),
            ParameterInOrOut = case InOrOut of 1 -> <<"收入">>; 2 -> <<"支出">>; 3 -> <<"其他">>; _ -> 0 end,
            MaxSearchSpan = application:get_env(restwong_cfg, max_fin_search_span, 366),
            TimeDiff = eadm_utils:time_diff(StartTime, EndTime),
            case TimeDiff > (MaxSearchSpan * 86400) of
                true ->
                    Alert = #{<<"Alert">> => unicode:characters_to_binary("查询时长超过 " ++ integer_to_list(MaxSearchSpan) ++ " 天，禁止查询!")},
                    {json, [Alert]};
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
                                Response = eadm_utils:pg_as_json(ResCol, ResData),
                                {json, Response};
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
                                Response = eadm_utils:pg_as_json(ResCol, ResData),
                                {json, Response};
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
                                Response = eadm_utils:pg_as_json(ResCol, ResData),
                                {json, Response};
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
                                Response = eadm_utils:pg_as_json(ResCol, ResData),
                                {json, Response}
                        end
                    catch
                        _:Error ->
                            lager:error("数据查询失败: ~p~n", [Error]),
                            Alert = #{<<"Alert">> => unicode:characters_to_binary("数据查询失败!")},
                            {json, [Alert]}
                    end
            end
    end;

search(#{auth_data := #{<<"permission">> := #{<<"finance">> := #{<<"finlist">> := false}}}}) ->
    Alert = #{<<"Alert">> => unicode:characters_to_binary("API鉴权失败!")},
    {json, [Alert]};

search(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 删除财务数据
%% @end
delete(#{auth_data := #{<<"authed">> := true, <<"loginname">> := LoginName,
      <<"permission">> := #{<<"finance">> := #{<<"findel">> := true}}},
    bindings := #{<<"detailId">> := DetailId}}) ->
    ParameterDetailId = binary_to_integer(DetailId),
        try
            eadm_pgpool:equery(pool_pg, "update fn_paybilldetail
                                      set deleteduser = $1,
                                      deletedat = current_timestamp,
                                      deleted = true
                                      where id = $2;",
                                      [LoginName, ParameterDetailId]),
            Info = #{<<"Alert">> => unicode:characters_to_binary("数据删成功! ")},
            {json, [Info]}
        catch
            _:Error ->
                lager:error("数据删除失败: ~p~n", [Error]),
                Alert = #{<<"Alert">> => unicode:characters_to_binary("数据删除失败!")},
                {json, [Alert]}
        end;

delete(#{auth_data := #{<<"permission">> := #{<<"finance">> := #{<<"findel">> := false}}}}) ->
    Alert = #{<<"Alert">> => unicode:characters_to_binary("API鉴权失败!")},
    {json, [Alert]};

delete(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 查询返回数据明细
%% @end
searchdetail(#{auth_data := #{<<"authed">> := true,
      <<"permission">> := #{<<"finance">> := #{<<"finlist">> := true}}},
    bindings := #{<<"detailId">> := DetailId}}) ->
    ParameterDetailId = binary_to_integer(DetailId),
    try
        {ok, ResCol, ResData} = eadm_pgpool:equery(pool_pg,
            "select owner, sourcetype, inorout, counterparty, counterbank, counteraccount,
               goodscomment, paymethod, amount, balance, currency, paystatus,
               tradetype, tradeorderno, counterorderno, tradetime, billcomment
             from fn_paybilldetail
             where deleted is false
               and id = $1;",
            [ParameterDetailId]),
        Response = eadm_utils:pg_as_map(ResCol, ResData),
        {json, Response}
    catch
        _:Error ->
            lager:error("用户信息查询失败: ~p~n", [Error]),
            Alert = #{<<"Alert">> => unicode:characters_to_binary("用户信息查询失败!")},
            {json, [Alert]}
    end;

searchdetail(#{auth_data := #{<<"permission">> := #{<<"finance">> := #{<<"finlist">> := false}}}}) ->
    Alert = #{<<"Alert">> => unicode:characters_to_binary("API鉴权失败!")},
    {json, [Alert]};

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
                            Alert = #{<<"Alert">> => unicode:characters_to_binary("数据插入失败!")},
                            {json, [Alert]}
                    end
                end,
                UploadJson),
            Info = #{<<"Alert">> => unicode:characters_to_binary("导入成功" ++ integer_to_list(count_maps(UploadJson)) ++ "行!")},
            {json, [Info]};
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
                            Alert = #{<<"Alert">> => unicode:characters_to_binary("数据插入失败!")},
                            {json, [Alert]}
                    end
                end,
                UploadJson),
            Info = #{<<"Alert">> => unicode:characters_to_binary("导入成功" ++ integer_to_list(count_maps(UploadJson)) ++ "行!")},
            {json, [Info]};
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
                            Alert = #{<<"Alert">> => unicode:characters_to_binary("数据插入失败!")},
                            {json, [Alert]}
                    end
                end,
                UploadJson),
            Info = #{<<"Alert">> => unicode:characters_to_binary("导入成功" ++ integer_to_list(count_maps(UploadJson)) ++ "行!")},
            {json, [Info]};
        <<"3">> ->
            lists:foreach(
                fun(Map) ->
                    InCome = maps:get(<<"Amount">>, Map),
                    case InCome of
                        _ when InCome > 0 ->
                        InOrOut = unicode:characters_to_binary("收入");
                        _ ->
                        InOrOut = unicode:characters_to_binary("支出")
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
                            Alert = #{<<"Alert">> => unicode:characters_to_binary("数据插入失败!")},
                            {json, [Alert]}
                    end
                end,
                UploadJson),
            Info = #{<<"Alert">> => unicode:characters_to_binary("导入成功" ++ integer_to_list(count_maps(UploadJson)) ++ "行!")},
            {json, [Info]};
        <<"4">> ->
            Info = #{<<"Alert">> => unicode:characters_to_binary("导入成功" ++ integer_to_list(count_maps(UploadJson)) ++ "行!")},
            {json, [Info]};
        _ ->
            {json, [unicode:characters_to_binary("导入格式不对!")]}
    end;

upload(#{auth_data := #{<<"permission">> := #{<<"finance">> := #{<<"finimp">> := false}}}}) ->
    Alert = #{<<"Alert">> => unicode:characters_to_binary("API鉴权失败!")},
    {json, [Alert]};

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
