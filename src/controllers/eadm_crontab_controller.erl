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
-export([index/1, search/1, add/1, update/1, delete/1, activate/1]).

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
    Alert = #{<<"Alert">> => unicode:characters_to_binary("API鉴权失败！", utf8)},
    {json, [Alert]};

index(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 查询任务信息
%% @end
search(#{auth_data := #{<<"authed">> := true, <<"permission">> := #{<<"crontab">> := true}},
    parsed_qs := #{<<"cronName">> := CronName}}) ->
    try
        CronNamePattern = <<"%", CronName/binary, "%">>,
        {ok, Res_Col, Res_Data} = eadm_pgpool:equery(pool_pg,
            "select id, cronname, crontype, cronexp, cronmfa,
            starttime, endtime, cronstatus, createdat
            from vi_crontab
            where cronname like $1
            order by createdat desc;",[CronNamePattern]),
        Response = eadm_utils:pg_as_json(Res_Col, Res_Data),
        {json, Response}
    catch
        _E:Error ->
            lager:error("任务查询失败：~p~n", [Error]),
            Alert = #{<<"Alert">> => unicode:characters_to_binary("任务查询失败！", utf8)},
            {json, [Alert]}
    end;

search(#{auth_data := #{<<"permission">> := #{<<"crontab">> := false}}}) ->
    Alert = #{<<"Alert">> => unicode:characters_to_binary("API鉴权失败！", utf8)},
    {json, [Alert]};

search(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 新增任务信息
%% @end
add(#{auth_data := #{<<"authed">> := true, <<"loginname">> := CreatedUser,
    <<"permission">> := #{<<"crontab">> := true}},
    params := #{<<"cronName">> := CronName, <<"cronType">> := CronType,
        <<"cronExp">> := CronExp, <<"cronModule">> := CronModule,
        <<"startTime">> := StartTime, <<"endTime">> := EndTime}}) ->
    try
        ParameterStartTime = eadm_utils:parse_date_time(StartTime),
        ParameterEndTime = eadm_utils:parse_date_time(EndTime),
        eadm_pgpool:equery(pool_pg, "insert into eadm_crontab(cronname, crontype, cronexp, cronmfa,
        starttime, endtime, createduser, updateduser) values($1, $2, $3, $4, $5, $6, $7, $8);",
        [CronName, CronType, CronExp, CronModule,
        ParameterStartTime, ParameterEndTime, CreatedUser, CreatedUser]),
        A = unicode:characters_to_binary("任务【", utf8),
        B = unicode:characters_to_binary("】新增成功！", utf8),
        Info = #{<<"Alert">> => <<A/binary, CronName/binary, B/binary>>},
        {json, [Info]}
    catch
        _E:Error ->
            lager:error("任务新增失败：~p~n", [Error]),
            Alert = #{<<"Alert">> => unicode:characters_to_binary("任务新增失败！", utf8)},
            {json, [Alert]}
    end;

add(#{auth_data := #{<<"permission">> := #{<<"crontab">> := false}}}) ->
    Alert = #{<<"Alert">> => unicode:characters_to_binary("API鉴权失败！", utf8)},
    {json, [Alert]};

add(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 更新任务信息
%% @end
update(#{auth_data := #{<<"authed">> := true, <<"permission">> := #{<<"crontab">> := true}},
    params := #{<<"cronId">> := CronId, <<"cronName">> := CronName, <<"cronType">> := CronType,
        <<"cronExp">> := CronExp, <<"cronModule">> := CronModule,
        <<"startTime">> := StartTime, <<"endTime">> := EndTime}}) ->
    ParameterStartTime = eadm_utils:parse_date_time(StartTime),
    ParameterEndTime = eadm_utils:parse_date_time(EndTime),
    try
        eadm_pgpool:equery(pool_pg,"update eadm_crontab set cronname=$1,crontype=2,cronexp=$3,
          cronmfa=$4,starttime=$5,endtime=$6 where id=$7;",
          [CronName, CronType, CronExp, CronModule, ParameterStartTime, ParameterEndTime, CronId])
    catch
        _E:Error ->
            lager:error("任务更新失败：~p~n", [Error]),
            Alert = #{<<"Alert">> => unicode:characters_to_binary("任务更新失败！", utf8)},
            {json, [Alert]}
    end,
    A = unicode:characters_to_binary("任务【", utf8),
    B = unicode:characters_to_binary("】更新成功！", utf8),
    Info = #{<<"Alert">> => <<A/binary, CronName/binary, B/binary>>},
    {json, [Info]};

update(#{auth_data := #{<<"permission">> := #{<<"crontab">> := false}}}) ->
    Alert = #{<<"Alert">> => unicode:characters_to_binary("API鉴权失败！", utf8)},
    {json, [Alert]};

update(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 删除任务信息
%% @end
delete(#{auth_data := #{<<"authed">> := true, <<"permission">> := #{<<"crontab">> := true}},
    bindings := #{<<"id">> := CronId}}) ->
    try
        eadm_pgpool:equery(pool_pg, "delete from eadm_crontab where id = $1;", [CronId])
    catch
        _E:Error ->
            lager:error("任务删除失败：~p~n", [Error]),
            Alert = #{<<"Alert">> => unicode:characters_to_binary("任务删除失败！", utf8)},
            {json, [Alert]}
    end,
    Info = #{<<"Alert">> => unicode:characters_to_binary("任务删除成功！", utf8)},
    {json, [Info]};

delete(#{auth_data := #{<<"permission">> := #{<<"crontab">> := false}}}) ->
    Alert = #{<<"Alert">> => unicode:characters_to_binary("API鉴权失败!", utf8)},
    {json, [Alert]};

delete(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 启禁用任务信息
%% @end
activate(#{auth_data := #{<<"authed">> := true, <<"permission">> := #{<<"crontab">> := true}},
    bindings := #{<<"id">> := CronId}}) ->
    try
        eadm_pgpool:equery(pool_pg, "update eadm_crontab set cornstatus=-1*cornstatus where id=$1;", [CronId])
    catch
        _E:Error ->
            lager:error("任务启禁用失败：~p~n", [Error]),
            Alert = #{<<"Alert">> => unicode:characters_to_binary("任务启禁用失败！", utf8)},
            {json, [Alert]}
    end,
    Info = #{<<"Alert">> => unicode:characters_to_binary("任务删除成功！", utf8)},
    {json, [Info]};

activate(#{auth_data := #{<<"permission">> := #{<<"crontab">> := false}}}) ->
    Alert = #{<<"Alert">> => unicode:characters_to_binary("API鉴权失败！", utf8)},
    {json, [Alert]};

activate(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%%====================================================================
%% 内部函数
%%====================================================================
