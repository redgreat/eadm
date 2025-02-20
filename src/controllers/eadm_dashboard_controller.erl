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
        {ok, _, ResData} = eadm_pgpool:equery(pool_pg,
            "with dt as (
                select unnest(array[1,2,3,4]) as datatype
            )
            select coalesce(d.datavalue, '0')
            from dt
            left join eadm_dashboard d
                on d.datatype = dt.datatype
                and d.loginname = $1
                and d.datavalue is not null
            order by dt.datatype;",[LoginName]),
        {ok, _, ResLocation} = eadm_pgpool:equery(pool_pg,
            "select cast(right(checkdate, 2) as int) as month, datavalue
            from eadm_dashboard
            where loginname = $1
                and datatype = 5
            order by cast(right(checkdate, 2) as int);",[LoginName]),
        {ok, _, ResFinanceIn} = eadm_pgpool:equery(pool_pg,
            "select cast(right(checkdate, 2) as int) as month, datavalue
            from eadm_dashboard
            where loginname = $1
                and datatype = 6
            order by cast(right(checkdate, 2) as int);",[LoginName]),
        {ok, _, ResFinanceOut} = eadm_pgpool:equery(pool_pg,
            "select cast(right(checkdate, 2) as int) as month, datavalue
            from eadm_dashboard
            where loginname = $1
                and datatype = 7
            order by cast(right(checkdate, 2) as int);",[LoginName]),
        DataValues = [V || {V} <- ResData],
        FinalData = DataValues ++          % resdata[0-3]: 周数据
        [0,0,0,0] ++                       % resdata[4-7]: 年数据, 先造个假数
        [get_hd(ResLocation)] ++           % resdata[8]: 地理位置月份标签
        [get_tl(ResLocation)] ++           % resdata[9]: 地理位置数据
        [get_hd(ResFinanceIn)] ++          % resdata[10]: 财务月份标签
        [get_tl(ResFinanceIn)] ++          % resdata[11]: 收入数据
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
get_hd(List) ->
    Mon = unicode:characters_to_binary("月", utf8),
    HdFun = fun({X, _}) -> Y = integer_to_binary(X), <<Y/binary, Mon/binary>> end,
    lists:map(HdFun, List).

get_tl(List) ->
    TlFun = fun({_, V}) -> V end,
    lists:map(TlFun, List).
