%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2024, REDGREAT
%%% @doc
%%%
%%% 定时任务逻辑处理
%%%
%%% @end
%%% Created : 2024-04-02 19:48:17
%%%-------------------------------------------------------------------
-module(eadm_crontab_controller).
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
search(#{auth_data := #{<<"authed">> := true}, bindings := #{<<"cronName">> := CronName}}) ->
    CronNamePattern = <<"%", CronName/binary, "%">>,
    {ok, Res_Col, Res_Data} = mysql_pool:query(pool_db,
        "SELECT CronName, CronExp, CronMFA, StartDateTime, EndDateTime, CronStatus, CreatedAt
        FROM eadm_crontab
        WHERE CronName LIKE ?
          AND Deleted=0
        ORDER BY CreatedAt DESC;",
        [CronNamePattern]),
    Response = eadm_utils:return_as_json(Res_Col, Res_Data),
    {json, Response};

search(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%%====================================================================
%% Internal functions
%%====================================================================
