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
            "select datavalue
            from eadm_dashboard
            where loginname = $1
              and datatype in (1, 2, 3, 4)
              and datavalue is not null
            order by datetype, datatype;",[LoginName]),
        {ok, _, ResLocation} = eadm_pgpool:equery(pool_pg,
            "select cast(right(checkdate, 2) as int) as month, datavalue
            from eadm_dashboard
            where loginname = $1
              and datatype = 5
            order by checkdate;",[LoginName]),
        {ok, _, ResFinanceIn} = eadm_pgpool:equery(pool_pg,
            "select cast(right(checkdate, 2) as int) as month, datavalue
            from eadm_dashboard
            where loginname = $1
              and datatype = 6
            order by checkdate;",[LoginName]),
        {ok, _, ResFinanceOut} = eadm_pgpool:equery(pool_pg,
            "select cast(right(checkdate, 2) as int) as month, datavalue
            from eadm_dashboard
            where loginname = $1
              and datatype = 7
            order by checkdate;",[LoginName]),
        {json, ResData ++ [get_hd(ResLocation)] ++ [get_tl(ResLocation)]
            ++ [get_hd(ResFinanceIn)] ++ [get_tl(ResFinanceIn)] ++ [get_tl(ResFinanceOut)]}
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
    HdFun = fun([X|_]) -> Y = erlang:integer_to_binary(X), <<Y/binary, Mon/binary>> end,
    lists:map(HdFun, List).

get_tl(List) ->
    TlFun = fun(X) -> erlang:list_to_binary(tl(X)) end,
    lists:map(TlFun, List).
