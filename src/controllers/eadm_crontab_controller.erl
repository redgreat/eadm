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
%%% 函数导出
%%%===================================================================
-export([index/1, search/1, add/1, update/1, delete/1, activate/1, deactivate/1, statistic/1]).

%%====================================================================
%% API 函数
%%====================================================================
%% @doc
%% 主函数
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
%% 查询任务信息
%% @end
search(#{auth_data := #{<<"authed">> := true, <<"permission">> := #{<<"crontab">> := true}},
      bindings := #{<<"cronName">> := CronName}}) ->
    CronNamePattern = <<"%", CronName/binary, "%">>,
    {ok, Res_Col, Res_Data} = eadm_pgpool:equery(pool_pg,
        "select cronname, cronexp, cronmfa, starttime, endtime, cronstatus, createdat
        from eadm_crontab
        where cronname like $1
          and deleted is false
        order by createdat desc;",
        [CronNamePattern]),
    Response = eadm_utils:pg_as_json(Res_Col, Res_Data),
    {json, Response};

search(#{auth_data := #{<<"permission">> := #{<<"crontab">> := false}}}) ->
    Alert = #{<<"Alert">> => unicode:characters_to_binary("API鉴权失败! ")},
    {json, [Alert]};

search(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 新增任务信息
%% @end
add(#{auth_data := #{<<"authed">> := true, <<"permission">> := #{<<"crontab">> := true}},
    bindings := #{<<"cronName">> := CronName}}) ->
    CronNamePattern = <<"%", CronName/binary, "%">>,
    {ok, Res_Col, Res_Data} = eadm_pgpool:equery(pool_pg,
        "select cronname, cronexp, cronmfa, starttime, endtime, cronstatus, createdat
        from eadm_crontab
        where cronname like $1
          and deleted is false
        order by createdat desc;",
        [CronNamePattern]),
    Response = eadm_utils:pg_as_json(Res_Col, Res_Data),
    {json, Response};

add(#{auth_data := #{<<"permission">> := #{<<"crontab">> := false}}}) ->
    Alert = #{<<"Alert">> => unicode:characters_to_binary("API鉴权失败! ")},
    {json, [Alert]};

add(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 更新任务信息
%% @end
update(#{auth_data := #{<<"authed">> := true, <<"permission">> := #{<<"crontab">> := true}},
    bindings := #{<<"cronName">> := CronName}}) ->
    CronNamePattern = <<"%", CronName/binary, "%">>,
    {ok, Res_Col, Res_Data} = eadm_pgpool:equery(pool_pg,
        "select cronname, cronexp, cronmfa, starttime, endtime, cronstatus, createdat
        from eadm_crontab
        where cronname like $1
          and deleted is false
        order by createdat desc;",
        [CronNamePattern]),
    Response = eadm_utils:pg_as_json(Res_Col, Res_Data),
    {json, Response};

update(#{auth_data := #{<<"permission">> := #{<<"crontab">> := false}}}) ->
    Alert = #{<<"Alert">> => unicode:characters_to_binary("API鉴权失败! ")},
    {json, [Alert]};

update(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 删除任务信息
%% @end
delete(#{auth_data := #{<<"authed">> := true, <<"permission">> := #{<<"crontab">> := true}},
    bindings := #{<<"cronName">> := CronName}}) ->
    CronNamePattern = <<"%", CronName/binary, "%">>,
    {ok, Res_Col, Res_Data} = eadm_pgpool:equery(pool_pg,
        "select cronname, cronexp, cronmfa, starttime, endtime, cronstatus, createdat
        from eadm_crontab
        where cronname like $1
          and deleted is false
        order by createdat desc;",
        [CronNamePattern]),
    Response = eadm_utils:pg_as_json(Res_Col, Res_Data),
    {json, Response};

delete(#{auth_data := #{<<"permission">> := #{<<"crontab">> := false}}}) ->
    Alert = #{<<"Alert">> => unicode:characters_to_binary("API鉴权失败! ")},
    {json, [Alert]};

delete(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 启动任务信息
%% @end
activate(#{auth_data := #{<<"authed">> := true, <<"permission">> := #{<<"crontab">> := true}},
    bindings := #{<<"cronName">> := CronName}}) ->
    CronNamePattern = <<"%", CronName/binary, "%">>,
    {ok, Res_Col, Res_Data} = eadm_pgpool:equery(pool_pg,
        "select cronname, cronexp, cronmfa, starttime, endtime, cronstatus, createdat
        from eadm_crontab
        where cronname like $1
          and deleted is false
        order by createdat desc;",
        [CronNamePattern]),
    Response = eadm_utils:pg_as_json(Res_Col, Res_Data),
    {json, Response};

activate(#{auth_data := #{<<"permission">> := #{<<"crontab">> := false}}}) ->
    Alert = #{<<"Alert">> => unicode:characters_to_binary("API鉴权失败! ")},
    {json, [Alert]};

activate(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 停止任务信息
%% @end
deactivate(#{auth_data := #{<<"authed">> := true, <<"permission">> := #{<<"crontab">> := true}},
    bindings := #{<<"cronName">> := CronName}}) ->
    CronNamePattern = <<"%", CronName/binary, "%">>,
    {ok, Res_Col, Res_Data} = eadm_pgpool:equery(pool_pg,
        "select cronname, cronexp, cronmfa, starttime, endtime, cronstatus, createdat
        from eadm_crontab
        where cronname like $1
          and deleted is false
        order by createdat desc;",
        [CronNamePattern]),
    Response = eadm_utils:pg_as_json(Res_Col, Res_Data),
    {json, Response};

deactivate(#{auth_data := #{<<"permission">> := #{<<"crontab">> := false}}}) ->
    Alert = #{<<"Alert">> => unicode:characters_to_binary("API鉴权失败! ")},
    {json, [Alert]};

deactivate(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 刷新任务信息
%% @end
statistic(#{auth_data := #{<<"authed">> := true, <<"permission">> := #{<<"crontab">> := true}},
    bindings := #{<<"cronName">> := CronName}}) ->
    CronNamePattern = <<"%", CronName/binary, "%">>,
    {ok, Res_Col, Res_Data} = eadm_pgpool:equery(pool_pg,
        "select cronname, cronexp, cronmfa, starttime, endtime, cronstatus, createdat
        from eadm_crontab
        where cronname like $1
          and deleted is false
        order by createdat desc;",
        [CronNamePattern]),
    Response = eadm_utils:pg_as_json(Res_Col, Res_Data),
    {json, Response};

statistic(#{auth_data := #{<<"permission">> := #{<<"crontab">> := false}}}) ->
    Alert = #{<<"Alert">> => unicode:characters_to_binary("API鉴权失败! ")},
    {json, [Alert]};

statistic(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.
%%====================================================================
%% 内部函数
%%====================================================================
