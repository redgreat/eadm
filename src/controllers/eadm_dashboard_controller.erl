%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2024, REDGREAT
%%% @doc
%%%
%%% eadm 大屏看板
%%%
%%% @end
%%% Created : 2024-02-27 08:26:07
%%%-------------------------------------------------------------------
-module(eadm_dashboard_controller).
-author("wangcw").

%%%===================================================================
%%% 函数导出
%%%===================================================================
-export([index/1, search/1]).

%%====================================================================
%% API 函数
%%====================================================================

%% @doc
%% 主函数
%% @end
index(#{auth_data := #{<<"authed">> := true, <<"username">> := UserName}}) ->
    {ok, [{username, UserName}]};
index(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 查询返回数据结果
%% @end
search(#{auth_data := #{<<"authed">> := true, <<"loginname">> := LoginName}}) ->
    try
        % 使用缓存包装器，TTL 5分钟
        {ok, _, ResData} = eadm_pgpool_cached:equery_cached(
            pool_pg,
            "with dt as (\n"
            "                select unnest(array[1,2,3,4]) as datatype\n"
            "            )\n"
            "            select coalesce(d.datavalue, '0')\n"
            "            from dt\n"
            "            left join eadm_dashboard d\n"
            "                on d.datatype = dt.datatype\n"
            "                and d.loginname = $1\n"
            "                and d.datavalue is not null\n"
            "            order by dt.datatype;",
            [LoginName],
            300,
            {dashboard_data, LoginName}
        ),
        {ok, _, ResLocation} = eadm_pgpool_cached:equery_cached(
            pool_pg,
            "select cast(right(checkdate, 2) as int) as month, datavalue\n"
            "            from eadm_dashboard\n"
            "            where loginname = $1\n"
            "                and datatype = 5\n"
            "            order by cast(right(checkdate, 2) as int);",
            [LoginName],
            300,
            {dashboard_location, LoginName}
        ),
        {ok, _, ResFinanceIn} = eadm_pgpool_cached:equery_cached(
            pool_pg,
            "select cast(right(checkdate, 2) as int) as month, datavalue\n"
            "            from eadm_dashboard\n"
            "            where loginname = $1\n"
            "                and datatype = 6\n"
            "            order by cast(right(checkdate, 2) as int);",
            [LoginName],
            300,
            {dashboard_finance_in, LoginName}
        ),
        {ok, _, ResFinanceOut} = eadm_pgpool_cached:equery_cached(
            pool_pg,
            "select cast(right(checkdate, 2) as int) as month, datavalue\n"
            "            from eadm_dashboard\n"
            "            where loginname = $1\n"
            "                and datatype = 7\n"
            "            order by cast(right(checkdate, 2) as int);",
            [LoginName],
            300,
            {dashboard_finance_out, LoginName}
        ),
        DataValues = [V || {V} <- ResData],
        % resdata[0-3]: 周数据
        FinalData =
            DataValues ++
                % resdata[4-7]: 年数据, 先造个假数
                [0, 0, 0, 0] ++
                % resdata[8]: 地理位置月份标签
                [get_hd(ResLocation)] ++
                % resdata[9]: 地理位置数据
                [get_tl(ResLocation)] ++
                % resdata[10]: 财务月份标签
                [get_hd(ResFinanceIn)] ++
                % resdata[11]: 收入数据
                [get_tl(ResFinanceIn)] ++
                [get_tl(ResFinanceOut)],
        {json, FinalData}
    catch
        _:Error ->
            lager:error("首页信息查询失败：~p~n", [Error]),
            {json, [#{<<"Alert">> => unicode:characters_to_binary("首页信息查询失败！", utf8)}]}
    end;
search(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%%====================================================================
%% 内部函数
%%====================================================================
%% @private
%% @doc
%% 获取月份标签列表
%% @end
get_hd(List) ->
    Mon = unicode:characters_to_binary("月", utf8),
    HdFun = fun({X, _}) ->
        Y = integer_to_binary(X),
        <<Y/binary, Mon/binary>>
    end,
    lists:map(HdFun, List).

%% @private
%% @doc
%% 获取数据值列表
%% @end
get_tl(List) ->
    TlFun = fun({_, V}) -> V end,
    lists:map(TlFun, List).
