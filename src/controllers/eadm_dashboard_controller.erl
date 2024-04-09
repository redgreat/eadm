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
%%% Application callbacks
%%%===================================================================
-export([index/1, search/1]).


%%====================================================================
%% API functions
%%====================================================================

%% @doc
%% index
%% @end
index(#{auth_data := #{<<"authed">> := true, <<"username">> := UserName}}) ->
    {ok, [{username, UserName}]};

index(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 查询返回数据结果
%% @end
search(#{auth_data := #{<<"authed">> := true, <<"loginname">> := LoginName}}) ->

    {ok, _, ResData} = mysql_pool:query(pool_db,
        "SELECT DataValue
        FROM eadm_dashboard
        WHERE LoginName=?
          AND DataType IN (1, 2, 3, 4)
        ORDER BY DateType, DataType;",[LoginName]),

    {ok, _, ResLocation} = mysql_pool:query(pool_db,
        "SELECT CAST(RIGHT(CheckDate, 2) AS INT) AS Month, DataValue
        FROM eadm_dashboard
        WHERE LoginName=?
          AND DataType=5
        ORDER BY CheckDate;",[LoginName]),

    {ok, _, ResFinanceIn} = mysql_pool:query(pool_db,
        "SELECT CAST(RIGHT(CheckDate, 2) AS INT) AS Month, DataValue
        FROM eadm_dashboard
        WHERE LoginName=?
          AND DataType=6
        ORDER BY CheckDate;",[LoginName]),

    {ok, _, ResFinanceOut} = mysql_pool:query(pool_db,
        "SELECT CAST(RIGHT(CheckDate, 2) AS INT) AS Month, DataValue
        FROM eadm_dashboard
        WHERE LoginName=?
          AND DataType=7
        ORDER BY CheckDate;",[LoginName]),

    ResList = ResData ++ [get_hd(ResLocation)] ++ [get_tl(ResLocation)]
        ++ [get_hd(ResFinanceIn)] ++ [get_tl(ResFinanceIn)] ++ [get_tl(ResFinanceOut)],
    {json, ResList};

search(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%%====================================================================
%% Internal functions
%%====================================================================
get_hd(List) ->
    Mon = unicode:characters_to_binary("月"),
    HdFun = fun([X|_]) -> Y = integer_to_binary(X), <<Y/binary, Mon/binary>> end,
    lists:map(HdFun, List).

get_tl(List) ->
    TlFun = fun(X) -> list_to_binary(tl(X)) end,
    lists:map(TlFun, List).
