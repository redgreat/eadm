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
index(#{auth_data := #{<<"authed">> := true, <<"username">> := UserName,
      <<"permission">> := #{<<"crontab">> := true}}}) ->
    {ok, [{username, UserName}]};

index(#{auth_data := #{<<"permission">> := #{<<"crontab">> := false}}}) ->
    Alert = #{<<"Alert">> => unicode:characters_to_binary("API鉴权失败! ")},
    {json, [Alert]};

index(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 查询返回数据结果
%% @end
search(#{auth_data := #{<<"authed">> := true, <<"permission">> := #{<<"crontab">> := true}},
      bindings := #{<<"cronName">> := CronName}}) ->
    CronNamePattern = <<"%", CronName/binary, "%">>,
    {ok, Res_Col, Res_Data} = eadm_pgpool:equery(pool_pg,
        "select cronname, cronexp, cronmda, startdatetime, enddatetime, cronstatus, createdat
        from eadm_crontab
        where cronname like ?
          and deleted is false
        order by createdat desc;",
        [CronNamePattern]),
    Response = eadm_utils:return_as_json(Res_Col, Res_Data),
    {json, Response};

search(#{auth_data := #{<<"permission">> := #{<<"crontab">> := false}}}) ->
    Alert = #{<<"Alert">> => unicode:characters_to_binary("API鉴权失败! ")},
    {json, [Alert]};

search(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%%====================================================================
%% Internal functions
%%====================================================================
