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
-export([init/0]).

%%%===================================================================
%%% API 函数
%%%====================================================================

%% @doc
%% 初始化函数，启动时调用
%% @end
init() ->
    application:ensure_all_started(ecron),
    try
        % 从数据库加载所有激活的定时任务
        {ok, _, ResData} = eadm_pgpool:equery(pool_pg,
            "select id, cronname, cronexp, cronmfa, starttime, endtime 
             from eadm_crontab 
             where cronstatus = 0 
              and deleted = false;", []),
        #{data := Jobs} = eadm_utils:pg_as_json([], ResData),
        lists:foreach(fun(Job) ->
            try
                schedule_job(Job)
            catch
                _:ErrorReason:_ ->
                    lager:error("初始化任务失败: ~p~n任务数据: ~p", 
                               [ErrorReason, Job])
            end
        end, Jobs),
        ok
    catch
        _:ErrorReason:_ ->
            lager:error("从数据库加载任务失败: ~p", [ErrorReason]),
            ok
    end.

%% @doc
%% 调度任务函数
%% @end
schedule_job(#{<<"id">> := Id, <<"cronexp">> := CronExp, <<"cronmfa">> := CronMFA} = Job) ->
    try
        JobId = list_to_atom("job_" ++ binary_to_list(Id)),
        StartTime = maps:get(<<"starttime">>, Job, undefined),
        EndTime = maps:get(<<"endtime">>, Job, undefined),
        
        % 解析 Erlang M:F/A 格式的字符串
        [ModStr, FunStr, ArgsStr] = binary:split(CronMFA, [<<":">>, <<"/">>], [global]),
        Mod = binary_to_atom(ModStr, utf8),
        Fun = binary_to_atom(FunStr, utf8),
        Args = parse_args(ArgsStr),
        
        % 验证模块和函数是否存在
        case code:ensure_loaded(Mod) of
            {module, _} ->
                case erlang:function_exported(Mod, Fun, length(Args)) of
                    true ->
                        JobFun = fun() -> apply(Mod, Fun, Args) end,
                        Options = [{id, JobId}, {start_time, StartTime}, {end_time, EndTime}],
                        ecron:add_job(JobId, CronExp, JobFun, Options);
                    false ->
                        lager:error("函数不存在: ~p:~p/~p", [Mod, Fun, length(Args)]),
                        {error, function_not_found}
                end;
            LoadError ->
                lager:error("模块加载失败: ~p (~p)", [Mod, LoadError]),
                {error, module_not_found}
        end
    catch
        error:{badmatch, _} ->
            lager:error("任务 MFA 格式错误: ~p", [CronMFA]),
            {error, invalid_mfa_format};
        ErrorType:ErrorReason:Stacktrace ->
            lager:error("调度任务失败: ~p:~p~n~p~n任务数据: ~p", 
                       [ErrorType, ErrorReason, Stacktrace, Job]),
            {error, {ErrorType, ErrorReason}}
    end;
schedule_job(Job) ->
    lager:error("任务数据格式不正确: ~p", [Job]),
    {error, invalid_job_format}.

%% @doc
%% 解析参数字符串
%% @end
parse_args(ArgsStr) ->
    try
        {ok, Tokens, _} = erl_scan:string(binary_to_list(ArgsStr) ++ "."),
        {ok, Args} = erl_parse:parse_term(Tokens),
        Args
    catch
        _:_ -> []
    end.

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
        {ok, Columns, ResData} = eadm_pgpool:equery(pool_pg,
            "select id, cronname, cronexp, cronmfa,
            starttime, endtime, cronstatus, createdat
            from vi_crontab
            where cronname like $1
            order by createdat desc;",[<<"%", CronName/binary, "%">>]),
        {json, eadm_utils:pg_as_json(Columns, ResData)}
    catch
        ErrorType:ErrorReason:Stacktrace ->
            lager:error("任务查询失败：~p:~p~n~p~n", [ErrorType, ErrorReason, Stacktrace]),
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
        {ok, _, ResData} = eadm_pgpool:equery(pool_pg,
            "select b.cronname, a.cronlog, to_char(a.exectime, 'yyyy-mm-dd hh24:mi:ss') as exectime
            from sys_cronlog a,
                 eadm_crontab b
            where a.cronid = b.id
              and a.cronid = $1
              and b.deleted is false
            order by a.exectime desc;",[CronId]),
        {json, eadm_utils:pg_as_json([], ResData)}
    catch
        _:ErrorReason:_ ->
            lager:error("任务查询失败：~p~n", [ErrorReason]),
            {json, [#{<<"Alert">> => unicode:characters_to_binary("任务查询失败！", utf8)}]}
    end;
detail(#{auth_data := #{<<"permission">> := #{<<"crontab">> := false}}}) ->
    {json, [#{<<"Alert">> => unicode:characters_to_binary("API鉴权失败！", utf8)}]};
detail(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 添加定时任务
%% @end
add(#{auth_data := #{<<"authed">> := true, <<"loginname">> := LoginName,
    <<"permission">> := #{<<"crontab">> := true}},
    params := #{<<"cronName">> := CronName, <<"cronExp">> := CronExp, <<"cronModule">> := CronModule,
        <<"startTime">> := StartTime, <<"endTime">> := EndTime}}) ->
    try
        CreatedUser = LoginName,
        Result = case EndTime of
            <<>> ->
                eadm_pgpool:equery(pool_pg, 
                    "insert into eadm_crontab(cronname, cronexp, cronmfa,
                        starttime, createduser, updateduser) 
                    values($1, $2, $3, $4, $5, $6) returning id;",
                    [CronName, CronExp, CronModule, 
                     eadm_utils:parse_date_time(StartTime), CreatedUser, CreatedUser]);
            _ ->
                eadm_pgpool:equery(pool_pg, 
                    "insert into eadm_crontab(cronname, cronexp, cronmfa,
                        starttime, endtime, createduser, updateduser) 
                    values($1, $2, $3, $4, $5, $6, $7) returning id;",
                    [CronName, CronExp, CronModule, 
                     eadm_utils:parse_date_time(StartTime),
                     eadm_utils:parse_date_time(EndTime), CreatedUser, CreatedUser])
        end,
        case Result of
            {ok, _, [{Id}]} ->
                {json, #{
                    status => true,
                    message => <<"定时任务添加成功"/utf8>>,
                    id => Id,
                    data => #{
                    <<"id">> => Id,
                    <<"cronname">> => CronName,
                    <<"cronexp">> => CronExp,
                    <<"cronmfa">> => CronModule,
                    <<"starttime">> => StartTime,
                    <<"endtime">> => EndTime
                    },
                    refresh => true
                }};
            _ ->
                {json, #{
                    status => false,
                    message => <<"定时任务添加失败，请重试"/utf8>>
                }}
        end
    catch
        ErrorType:ErrorReason:Stacktrace ->
            lager:error("任务添加失败：~p:~p~n~p~n", [ErrorType, ErrorReason, Stacktrace]),
            {json, #{
                status => false,
                message => <<"定时任务添加失败，请联系管理员"/utf8>>
            }}
    end;
add(#{auth_data := #{<<"permission">> := #{<<"crontab">> := false}}}) ->
    {json, [#{<<"Alert">> => unicode:characters_to_binary("API鉴权失败！", utf8)}]};

add(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.


%% @doc
%% 编辑定时任务
%% @end
edit(#{auth_data := #{<<"authed">> := true, <<"loginname">> := LoginName,
    <<"permission">> := #{<<"crontab">> := true}},
    params := #{<<"cronId">> := CronId, <<"cronName">> := CronName, <<"cronExp">> := CronExp, <<"cronModule">> := CronModule,
        <<"startTime">> := StartTime, <<"endTime">> := EndTime}}) ->
    case EndTime of
        <<>> ->
            ParameterStartTime = eadm_utils:parse_date_time(StartTime),
            try
                eadm_pgpool:equery(pool_pg,"update eadm_crontab set cronname=$1,cronexp=$3,
                  cronmfa=$4,starttime=$5,updateduser=$6 where id=$7;",
                  [CronName, CronExp, CronModule, ParameterStartTime, LoginName, CronId])
            catch
                ErrorType:ErrorReason:Stacktrace ->
                    lager:error("任务更新失败：~p:~p~n~p~n", [ErrorType, ErrorReason, Stacktrace]),
                    {json, #{
                        status => false,
                        message => <<"定时任务更新失败，请联系管理员"/utf8>>
                    }}
            end;
        _ ->
            ParameterStartTime = eadm_utils:parse_date_time(StartTime),
            ParameterEndTime = eadm_utils:parse_date_time(EndTime),
            try
                eadm_pgpool:equery(pool_pg,"update eadm_crontab set cronname=$1,cronexp=$3,
                  cronmfa=$4,starttime=$5,endtime=$6,updateduser=$7 where id=$8;",
                    [CronName, CronExp, CronModule, ParameterStartTime, ParameterEndTime, LoginName, CronId])
            catch
                ErrorType:ErrorReason:Stacktrace ->
                    lager:error("任务更新失败：~p:~p~n~p~n", [ErrorType, ErrorReason, Stacktrace]),
                    {json, #{
                        status => false,
                        message => <<"定时任务更新失败，请联系管理员"/utf8>>
                    }}
            end
    end,
    {json, #{
        status => true,
        message => <<"定时任务更新成功"/utf8>>,
        refresh => true
    }};

edit(#{auth_data := #{<<"permission">> := #{<<"crontab">> := false}}}) ->
    {json, [#{<<"Alert">> => unicode:characters_to_binary("API鉴权失败！", utf8)}]};

edit(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 激活或停用定时任务
%% @end
activate(#{auth_data := #{<<"authed">> := true, <<"loginname">> := LoginName,
    <<"permission">> := #{<<"crontab">> := true}},
    params := #{<<"cronId">> := CronId, <<"status">> := Status}}) ->
    try
        {ok, _, ResData} = eadm_pgpool:equery(pool_pg,
            "update eadm_crontab set cronstatus = $1, updateduser = $2, updatedat = now() 
             where id = $3 returning cronexp, cronmfa, starttime, endtime;",
            [Status, LoginName, CronId]),
        case Status of
            true ->
                % 激活任务，添加到调度器
                case ResData of
                    [{CronExp, CronMFA, StartTime, EndTime}] ->
                        Job = #{
                            <<"id">> => CronId,
                            <<"cronexp">> => CronExp,
                            <<"cronmfa">> => CronMFA,
                            <<"starttime">> => StartTime,
                            <<"endtime">> => EndTime
                        },
                        schedule_job(Job),
                        {json, #{
                            status => true,
                            message => <<"任务已激活"/utf8>>,
                            refresh => true
                        }};
                    _ ->
                        {json, #{
                            status => false,
                            message => <<"任务激活失败，请确保任务类型为 Erlang"/utf8>>
                        }}
                end;
            false ->
                % 停用任务，从调度器中移除
                JobId = list_to_atom("job_" ++ binary_to_list(CronId)),
                ecron:delete_job(JobId),
                {json, #{
                    status => true,
                    message => <<"任务已停用"/utf8>>,
                    refresh => true
                }}
        end
    catch
        ErrorType:ErrorReason:Stacktrace ->
            lager:error("任务状态更新失败：~p:~p~n~p~n", [ErrorType, ErrorReason, Stacktrace]),
            {json, #{
                status => false,
                message => <<"任务状态更新失败，请联系管理员"/utf8>>
            }}
    end;

activate(#{auth_data := #{<<"permission">> := #{<<"crontab">> := false}}}) ->
    {json, [#{<<"Alert">> => unicode:characters_to_binary("API鉴权失败！", utf8)}]};

activate(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 删除定时任务
%% @end
delete(#{auth_data := #{<<"authed">> := true, <<"loginname">> := UpdatedUser,
    <<"permission">> := #{<<"crontab">> := true}},
    bindings := #{<<"cronId">> := CronId}}) ->
    try
        {ok, _, _} = eadm_pgpool:equery(pool_pg,
            "update eadm_crontab set deleted = true, updateduser = $1, updatedat = now() where id = $2;",
            [UpdatedUser, CronId]),
        JobId = list_to_atom("job_" ++ binary_to_list(CronId)),
        ecron:delete_job(JobId),
        {json, [#{<<"Success">> => unicode:characters_to_binary("任务删除成功！", utf8)}]}
    catch
        ErrorType:ErrorReason:Stacktrace ->
            lager:error("任务删除失败：~p:~p~n~p~n", [ErrorType, ErrorReason, Stacktrace]),
            {json, [#{<<"Alert">> => unicode:characters_to_binary("任务删除失败！", utf8)}]}
    end;

delete(#{auth_data := #{<<"permission">> := #{<<"crontab">> := false}}}) ->
    {json, [#{<<"Alert">> => unicode:characters_to_binary("API鉴权失败！", utf8)}]};

delete(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%%====================================================================
%% 内部函数
%%====================================================================
