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
-export([index/1, search/1, detail/1, add/1, edit/1, delete/1, activate/1]).

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
    {json, [#{<<"Alert">> => unicode:characters_to_binary("API鉴权失败！", utf8)}]};

index(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 查询任务信息
%% @end
search(#{auth_data := #{<<"authed">> := true, <<"permission">> := #{<<"crontab">> := true}},
    parsed_qs := #{<<"cronName">> := CronName}}) ->
    try
        {ok, ResCol, ResData} = eadm_pgpool:equery(pool_pg,
            "select id, cronname, crontype, cronexp, cronmfa,
            starttime, endtime, cronstatus, createdat
            from vi_crontab
            where cronname like $1
            order by createdat desc;",[<<"%", CronName/binary, "%">>]),
        {json, eadm_utils:pg_as_json(ResCol, ResData)}
    catch
        _:Error ->
            lager:error("任务查询失败：~p~n", [Error]),
            {json, [#{<<"Alert">> => unicode:characters_to_binary("任务查询失败！", utf8)}]}
    end;

search(#{auth_data := #{<<"permission">> := #{<<"crontab">> := false}}}) ->
    {json, [#{<<"Alert">> => unicode:characters_to_binary("API鉴权失败！", utf8)}]};

search(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 查询任务运行日志
%% @end
detail(#{auth_data := #{<<"authed">> := true, <<"permission">> := #{<<"crontab">> := true}},
    bindings := #{<<"cronId">> := CronId}}) ->
    try
        {ok, ResCol, ResData} = eadm_pgpool:equery(pool_pg,
            "select b.cronname, a.cronlog, to_char(a.exectime, 'yyyy-mm-dd hh24:mi:ss') as exectime
            from sys_cronlog a,
                 eadm_crontab b
            where a.cronid=b.id
              and a.cronid = $1
              and b.deleted is false
            order by a.exectime desc;",[CronId]),
        {json, eadm_utils:pg_as_json(ResCol, ResData)}
    catch
        _:Error ->
            lager:error("任务查询失败：~p~n", [Error]),
            {json, [#{<<"Alert">> => unicode:characters_to_binary("任务查询失败！", utf8)}]}
    end;

detail(#{auth_data := #{<<"permission">> := #{<<"crontab">> := false}}}) ->
    {json, [#{<<"Alert">> => unicode:characters_to_binary("API鉴权失败！", utf8)}]};

detail(#{auth_data := #{<<"authed">> := false}}) ->
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
        case EndTime of
            <<>> ->
                eadm_pgpool:equery(pool_pg, "insert into eadm_crontab(cronname, crontype, cronexp, cronmfa,
                    starttime, createduser, updateduser) values($1, $2, $3, $4, $5, $6, $7);",
                    [CronName, CronType, CronExp, CronModule, eadm_utils:parse_date_time(StartTime), CreatedUser, CreatedUser]);
            _ ->
                eadm_pgpool:equery(pool_pg, "insert into eadm_crontab(cronname, crontype, cronexp, cronmfa,
                    starttime, endtime, createduser, updateduser) values($1, $2, $3, $4, $5, $6, $7, $8);",
                    [CronName, CronType, CronExp, CronModule, eadm_utils:parse_date_time(StartTime),
                        eadm_utils:parse_date_time(EndTime), CreatedUser, CreatedUser])
        end,
        A = unicode:characters_to_binary("任务【", utf8),
        B = unicode:characters_to_binary("】新增成功！", utf8),
        {json, [#{<<"Alert">> => <<A/binary, CronName/binary, B/binary>>}]}
    catch
        _:Error ->
            lager:error("任务新增失败：~p~n", [Error]),
            {json, [#{<<"Alert">> => unicode:characters_to_binary("任务新增失败！", utf8)}]}
    end;

add(#{auth_data := #{<<"permission">> := #{<<"crontab">> := false}}}) ->
    {json, [#{<<"Alert">> => unicode:characters_to_binary("API鉴权失败！", utf8)}]};

add(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 更新任务信息
%% @end
edit(#{auth_data := #{<<"authed">> := true, <<"loginname">> := LoginName,
    <<"permission">> := #{<<"crontab">> := true}},
    params := #{<<"cronId">> := CronId, <<"cronName">> := CronName, <<"cronType">> := CronType,
        <<"cronExp">> := CronExp, <<"cronModule">> := CronModule,
        <<"startTime">> := StartTime, <<"endTime">> := EndTime}}) ->
    case EndTime of
        <<>> ->
            ParameterStartTime = eadm_utils:parse_date_time(StartTime),
            try
                eadm_pgpool:equery(pool_pg,"update eadm_crontab set cronname=$1,crontype=$2,cronexp=$3,
                  cronmfa=$4,starttime=$5,updateduser=$6 where id=$7;",
                  [CronName, CronType, CronExp, CronModule, ParameterStartTime, LoginName, CronId])
            catch
                _:Error ->
                    lager:error("任务更新失败：~p~n", [Error]),
                    {json, [#{<<"Alert">> => unicode:characters_to_binary("任务更新失败！", utf8)}]}
            end;
        _ ->
            ParameterStartTime = eadm_utils:parse_date_time(StartTime),
            ParameterEndTime = eadm_utils:parse_date_time(EndTime),
            try
                eadm_pgpool:equery(pool_pg,"update eadm_crontab set cronname=$1,crontype=$2,cronexp=$3,
                  cronmfa=$4,starttime=$5,endtime=$6,updateduser=$7 where id=$8;",
                    [CronName, CronType, CronExp, CronModule, ParameterStartTime, ParameterEndTime, LoginName, CronId])
            catch
                _:Error ->
                    lager:error("任务更新失败：~p~n", [Error]),
                    {json, [#{<<"Alert">> => unicode:characters_to_binary("任务更新失败！", utf8)}]}
            end
    end,
    A = unicode:characters_to_binary("任务【", utf8),
    B = unicode:characters_to_binary("】更新成功！", utf8),
    {json, [#{<<"Alert">> => <<A/binary, CronName/binary, B/binary>>}]};

edit(#{auth_data := #{<<"permission">> := #{<<"crontab">> := false}}}) ->
    {json, [#{<<"Alert">> => unicode:characters_to_binary("API鉴权失败！", utf8)}]};

edit(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 启禁用任务信息
%% @end
activate(#{auth_data := #{<<"authed">> := true, <<"loginname">> := LoginName,
    <<"permission">> := #{<<"crontab">> := true}},
    bindings := #{<<"cronId">> := CronId}}) ->
    try
        eadm_pgpool:equery(pool_pg, "update eadm_crontab set cronstatus=abs(cronstatus-1), updateduser=$1
        where id=$2;", [LoginName, CronId])
    catch
        _:Error ->
            lager:error("任务启禁用失败：~p~n", [Error]),
            {json, [#{<<"Alert">> => unicode:characters_to_binary("任务启禁用失败！", utf8)}]}
    end,
    {json, [#{<<"Alert">> => unicode:characters_to_binary("任务启禁用成功！", utf8)}]};

activate(#{auth_data := #{<<"permission">> := #{<<"crontab">> := false}}}) ->
    {json, [#{<<"Alert">> => unicode:characters_to_binary("API鉴权失败！", utf8)}]};

activate(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 删除任务信息
%% @end
delete(#{auth_data := #{<<"authed">> := true, <<"loginname">> := LoginName,
    <<"permission">> := #{<<"crontab">> := true}},
    bindings := #{<<"cronId">> := CronId}}) ->
    try
        eadm_pgpool:equery(pool_pg, "update eadm_crontab
        set deleteduser=$1,deletedat=current_timestamp,deleted=true
        where id=$2 and deleted is false;", [LoginName, CronId])
    catch
        _:Error ->
            lager:error("任务删除失败：~p~n", [Error]),
            {json, [#{<<"Alert">> => unicode:characters_to_binary("任务删除失败！", utf8)}]}
    end,
    {json, [#{<<"Alert">> => unicode:characters_to_binary("任务删除成功！", utf8)}]};

delete(#{auth_data := #{<<"permission">> := #{<<"crontab">> := false}}}) ->
    {json, [#{<<"Alert">> => unicode:characters_to_binary("API鉴权失败!", utf8)}]};

delete(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%%====================================================================
%% 内部函数
%%====================================================================
