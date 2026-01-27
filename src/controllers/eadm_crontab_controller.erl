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

-include("eadm_mnesia.hrl").

%%%===================================================================
%%% 函数导出
%%%===================================================================
-export([index/1, search/1, detail/1, add/1, edit/1, delete/1, toggle/1]).
-export([init/0]).

%%%===================================================================
%%% API 函数
%%%====================================================================

%% @doc
%% 初始化函数，启动时调用
%% @end
init() ->
    lager:info("开始初始化定时任务系统"),

    % 检查 ecron 是否可用
    case code:ensure_loaded(ecron) of
        {module, _} ->
            case application:ensure_all_started(ecron) of
                {ok, _} ->
                    timer:sleep(1000),
                    load_and_schedule_jobs();
                {error, StartError} ->
                    lager:error("ecron 应用启动失败: ~p，跳过定时任务初始化", [StartError])
            end;
        LoadError ->
            lager:error("ecron 模块加载失败: ~p，跳过定时任务初始化", [LoadError])
    end,
    ok.

%% @doc
%% 加载并调度任务
%% @end
-spec load_and_schedule_jobs() -> ok.
load_and_schedule_jobs() ->
    try
        % 从 Mnesia 加载所有激活的定时任务
        AllJobs = eadm_mnesia_api:find_by_field(eadm_crontab, cronstatus, 0),
        % 过滤已删除的任务并转换为 map
        ActiveJobs = [
            #{
                <<"id">> => Id,
                <<"cronname">> => Name,
                <<"cronexp">> => Exp,
                <<"cronmfa">> => MFA,
                <<"starttime">> => Start,
                <<"endtime">> => End
            }
         || #eadm_crontab{
                id = Id,
                cronname = Name,
                cronexp = Exp,
                cronmfa = MFA,
                starttime = Start,
                endtime = End,
                deleted = false
            } <- AllJobs
        ],

        case ActiveJobs of
            [] ->
                lager:info("没有找到需要初始化的定时任务"),
                ok;
            Jobs ->
                lager:info("找到 ~p 个需要初始化的定时任务", [length(Jobs)]),
                lists:foreach(
                    fun(#{<<"cronname">> := CronName} = Job) ->
                        try
                            ScheduleResult = schedule_job(Job),
                            case ScheduleResult of
                                {ok, _} ->
                                    lager:info("任务 ~p 初始化成功", [CronName]);
                                {error, ScheduleError} ->
                                    lager:error("任务 ~p 初始化失败: ~p", [CronName, ScheduleError]);
                                OtherResult ->
                                    lager:info("任务 ~p 初始化结果: ~p", [CronName, OtherResult])
                            end
                        catch
                            InitErrorType:InitErrorReason:InitStacktrace ->
                                lager:error(
                                    "初始化任务失败: ~p:~p~n~p~n任务数据: ~p",
                                    [InitErrorType, InitErrorReason, InitStacktrace, Job]
                                )
                        end
                    end,
                    Jobs
                ),
                ok
        end
    catch
        ErrorType:ErrorReason:Stacktrace ->
            lager:error("从 Mnesia 加载任务失败: ~p:~p~n~p", [ErrorType, ErrorReason, Stacktrace]),
            ok
    end.

%% @doc
%% 调度任务函数
%% @end
-spec schedule_job(map()) -> any().
schedule_job(#{<<"id">> := Id, <<"cronexp">> := CronExp, <<"cronmfa">> := CronMFA} = Job) when
    is_binary(Id), is_binary(CronExp), is_binary(CronMFA)
->
    try
        % 检查 cronmfa 格式是否有效
        case is_valid_mfa_format(CronMFA) of
            false ->
                lager:error("任务 ~p 的 MFA 格式无效: ~p，跳过调度", [Id, CronMFA]),
                {error, invalid_mfa_format};
            true ->
                % 创建唯一的任务ID
                JobId = list_to_atom("job_" ++ binary_to_list(Id)),

                % 获取开始和结束时间
                StartTime = eadm_utils:parse_time(maps:get(<<"starttime">>, Job, undefined)),
                EndTime = eadm_utils:parse_time(maps:get(<<"endtime">>, Job, undefined)),

                % 解析 Erlang M:F/A 格式的字符串
                [ModStr, FunStr, ArgsStr] = binary:split(CronMFA, [<<":">>, <<"/">>], [global]),
                Mod = binary_to_atom(ModStr, utf8),
                Fun = binary_to_atom(FunStr, utf8),
                Args = parse_args(ArgsStr),

                lager:info(
                    "解析MFA: ~p:~p/~p -> ~p:~p(~p)",
                    [ModStr, FunStr, ArgsStr, Mod, Fun, Args]
                ),

                case code:ensure_loaded(Mod) of
                    {module, _} ->
                        case erlang:function_exported(Mod, Fun, length(Args)) of
                            true ->
                                JobFun = fun() ->
                                    erlang:put(current_job_id, Id),
                                    try
                                        apply(Mod, Fun, Args)
                                    catch
                                        ErrorType:ErrorReason:Stacktrace ->
                                            lager:error("任务执行失败: ~p:~p~n~p", [
                                                ErrorType, ErrorReason, Stacktrace
                                            ])
                                    end
                                end,

                                case code:ensure_loaded(ecron) of
                                    {module, _} ->
                                        ParsedStartTime =
                                            case StartTime of
                                                undefined -> unlimited;
                                                null -> unlimited;
                                                {{_, _, _}, {H1, M1, S1}} -> {H1, M1, S1};
                                                _ -> unlimited
                                            end,
                                        ParsedEndTime =
                                            case EndTime of
                                                undefined -> unlimited;
                                                null -> unlimited;
                                                {{_, _, _}, {H2, M2, S2}} -> {H2, M2, S2};
                                                _ -> unlimited
                                            end,
                                        Result = ecron:add(
                                            ecron_local,
                                            JobId,
                                            binary_to_list(CronExp),
                                            {erlang, apply, [JobFun, []]},
                                            ParsedStartTime,
                                            ParsedEndTime,
                                            [{singleton, true}]
                                        ),
                                        Result;
                                    LoadError ->
                                        lager:error("ecron 模块加载失败: ~p", [LoadError]),
                                        {error, ecron_not_available}
                                end;
                            false ->
                                lager:error("函数不存在: ~p:~p/~p", [Mod, Fun, length(Args)]),
                                {error, function_not_found}
                        end;
                    LoadError ->
                        lager:error("模块加载失败: ~p (~p)", [Mod, LoadError]),
                        {error, module_not_found}
                end
        end
    catch
        error:{badmatch, _} ->
            lager:error("任务 MFA 格式错误: ~p", [CronMFA]),
            {error, invalid_mfa_format};
        ErrorType:ErrorReason:Stacktrace ->
            lager:error(
                "调度任务失败: ~p:~p~n~p~n任务数据: ~p",
                [ErrorType, ErrorReason, Stacktrace, Job]
            ),
            {error, {ErrorType, ErrorReason}}
    end;
schedule_job(Job) ->
    lager:error("任务数据格式不正确: ~p", [Job]),
    {error, invalid_job_format}.

%% @doc
%% 检查 MFA 格式是否有效
%% @end
is_valid_mfa_format(CronMFA) when is_binary(CronMFA) ->
    try
        % 检查是否包含 : 和 / 分隔符
        case binary:split(CronMFA, [<<":">>, <<"/">>], [global]) of
            [_ModStr, _FunStr, _ArgsStr] ->
                true;
            _ ->
                false
        end
    catch
        _:_ ->
            false
    end;
is_valid_mfa_format(_) ->
    false.

%% @doc
%% 解析参数字符串
%% @end
parse_args(ArgsStr) ->
    try
        % 如果参数字符串是数字，直接返回一个包含该数字的列表
        case re:run(ArgsStr, <<"^\\d+$">>) of
            {match, _} ->
                % 是纯数字，转换为整数并包装成列表
                Num = binary_to_integer(ArgsStr),
                [Num];
            nomatch ->
                % 尝试解析为 Erlang 项
                {ok, Tokens, _} = erl_scan:string(binary_to_list(ArgsStr) ++ "."),
                {ok, Args} = erl_parse:parse_term(Tokens),
                % 确保返回的是一个列表
                case is_list(Args) of
                    true -> Args;
                    % 如果不是列表，将其包装成列表
                    false -> [Args]
                end
        end
    catch
        ErrorType:ErrorReason:Stacktrace ->
            lager:info(
                "解析参数失败: ~p:~p~n~p~n参数字符串: ~p",
                [ErrorType, ErrorReason, Stacktrace, ArgsStr]
            ),
            % 返回空列表作为默认值
            []
    end.

%% @doc
%% 主函数
%% @end
index(#{
    auth_data := #{
        <<"authed">> := true,
        <<"username">> := UserName,
        <<"permission">> := #{<<"crontab">> := true}
    }
}) ->
    {ok, [{username, UserName}]};
index(#{auth_data := #{<<"permission">> := #{<<"crontab">> := false}}}) ->
    {json, [#{<<"Alert">> => unicode:characters_to_binary("API鉴权失败！", utf8)}]};
index(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 查询任务信息
%% @end
search(#{
    auth_data := #{<<"authed">> := true, <<"permission">> := #{<<"crontab">> := true}},
    parsed_qs := #{<<"cronName">> := CronName}
}) ->
    try
        % 使用缓存包装器，TTL 5分钟
        {ok, Columns, ResData} = eadm_pgpool_cached:equery_cached(
            pool_pg,
            "select id, cronname, cronexp, cronmfa,
\n"
            "            starttime, endtime, cronstatus, createdat
\n"
            "            from vi_crontab
\n"
            "            where cronname like $1
\n"
            "            order by createdat desc;",
            [<<"%", CronName/binary, "%">>],
            300,
            {crontab_list, CronName}
        ),
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
detail(#{
    auth_data := #{<<"authed">> := true, <<"permission">> := #{<<"crontab">> := true}},
    bindings := #{<<"cronId">> := CronId}
}) ->
    try
        {ok, _, ResData} = eadm_pgpool:equery(
            pool_pg,
            "select b.cronname, a.cronlog, to_char(a.exectime, 'yyyy-mm-dd hh24:mi:ss') as exectime
\n"
            "            from sys_cronlog a,
\n"
            "                 eadm_crontab b
\n"
            "            where a.cronid = b.id
\n"
            "              and a.cronid = $1
\n"
            "              and b.deleted is false
\n"
            "            order by a.exectime desc;",
            [CronId]
        ),
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
add(#{
    auth_data := #{
        <<"authed">> := true,
        <<"loginname">> := LoginName,
        <<"permission">> := #{<<"crontab">> := true}
    },
    params := #{
        <<"cronName">> := CronName,
        <<"cronExp">> := CronExp,
        <<"cronModule">> := CronModule,
        <<"startTime">> := StartTime,
        <<"endTime">> := EndTime
    }
}) ->
    try
        CreatedUser = LoginName,
        Result =
            case EndTime of
                <<>> ->
                    eadm_pgpool:equery(
                        pool_pg,
                        "insert into eadm_crontab(cronname, cronexp, cronmfa,
\n"
                        "                        starttime, createduser, updateduser)
\n"
                        "                    values($1, $2, $3, $4, $5, $6) returning id;",
                        [
                            CronName,
                            CronExp,
                            CronModule,
                            eadm_utils:parse_date_time(StartTime),
                            CreatedUser,
                            CreatedUser
                        ]
                    );
                _ ->
                    eadm_pgpool:equery(
                        pool_pg,
                        "insert into eadm_crontab(cronname, cronexp, cronmfa,
\n"
                        "                        starttime, endtime, createduser, updateduser)
\n"
                        "                    values($1, $2, $3, $4, $5, $6, $7) returning id;",
                        [
                            CronName,
                            CronExp,
                            CronModule,
                            eadm_utils:parse_date_time(StartTime),
                            eadm_utils:parse_date_time(EndTime),
                            CreatedUser,
                            CreatedUser
                        ]
                    )
            end,
        case Result of
            {ok, _, [{Id}]} ->
                % 默认新增的任务为未激活状态，需要用户手动激活
                {json, #{
                    status => true,
                    message => <<"定时任务添加成功，请手动激活任务"/utf8>>,
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
edit(#{
    auth_data := #{
        <<"authed">> := true,
        <<"loginname">> := LoginName,
        <<"permission">> := #{<<"crontab">> := true}
    },
    params := #{
        <<"cronId">> := CronId,
        <<"cronName">> := CronName,
        <<"cronExp">> := CronExp,
        <<"cronModule">> := CronModule,
        <<"startTime">> := StartTime,
        <<"endTime">> := EndTime
    }
}) ->
    try
        % 查询任务当前状态
        {ok, _, StatusData} = eadm_pgpool:equery(
            pool_pg,
            "select cronstatus from eadm_crontab where id = $1 and deleted is false;",
            [CronId]
        ),

        % 更新任务信息
        case EndTime of
            <<>> ->
                ParameterStartTime = eadm_utils:parse_date_time(StartTime),
                eadm_pgpool:equery(
                    pool_pg,
                    "update eadm_crontab set cronname=$1,cronexp=$2,
\n"
                    "                  cronmfa=$3,starttime=$4,updateduser=$5,updatedat=now() where id=$6;",
                    [CronName, CronExp, CronModule, ParameterStartTime, LoginName, CronId]
                );
            _ ->
                ParameterStartTime = eadm_utils:parse_date_time(StartTime),
                ParameterEndTime = eadm_utils:parse_date_time(EndTime),
                eadm_pgpool:equery(
                    pool_pg,
                    "update eadm_crontab set cronname=$1,cronexp=$2,
\n"
                    "                  cronmfa=$3,starttime=$4,endtime=$5,updateduser=$6,updatedat=now() where id=$7;",
                    [
                        CronName,
                        CronExp,
                        CronModule,
                        ParameterStartTime,
                        ParameterEndTime,
                        LoginName,
                        CronId
                    ]
                )
        end,

        % 如果任务当前是激活状态，则更新调度器中的任务
        case StatusData of
            [{true}] ->
                % 先从调度器中删除旧任务
                JobId = list_to_atom("job_" ++ binary_to_list(CronId)),
                try
                    ecron:delete(JobId)
                catch
                    _:_ -> ok
                end,

                % 添加新任务到调度器
                Job = #{
                    <<"id">> => CronId,
                    <<"cronexp">> => CronExp,
                    <<"cronmfa">> => CronModule,
                    <<"starttime">> => StartTime,
                    <<"endtime">> => EndTime
                },
                schedule_job(Job),
                {json, #{
                    status => true,
                    message => <<"定时任务更新成功并重新调度"/utf8>>,
                    refresh => true
                }};
            _ ->
                % 任务未激活，只更新数据库信息
                {json, #{
                    status => true,
                    message => <<"定时任务更新成功"/utf8>>,
                    refresh => true
                }}
        end
    catch
        ErrorType:ErrorReason:Stacktrace ->
            lager:error("任务更新失败：~p:~p~n~p~n", [ErrorType, ErrorReason, Stacktrace]),
            {json, #{
                status => false,
                message => <<"定时任务更新失败，请联系管理员"/utf8>>
            }}
    end;
edit(#{auth_data := #{<<"permission">> := #{<<"crontab">> := false}}}) ->
    {json, [#{<<"Alert">> => unicode:characters_to_binary("API鉴权失败！", utf8)}]};
edit(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 删除定时任务
%% @end
delete(#{
    auth_data := #{
        <<"authed">> := true,
        <<"loginname">> := UpdatedUser,
        <<"permission">> := #{<<"crontab">> := true}
    },
    bindings := #{<<"cronId">> := CronId}
}) ->
    try
        % 先从调度器中移除任务
        JobId = list_to_atom("job_" ++ binary_to_list(CronId)),
        try
            ecron:delete(JobId)
        catch
            _:_ -> ok
        end,

        % 然后在数据库中标记为删除
        Result = eadm_pgpool:equery(
            pool_pg,
            "update eadm_crontab set deleted = true,
\n"
            "             updateduser = $1, updatedat = now(),
\n"
            "             deleteduser = $1, deletedat = now()
\n"
            "             where id = $2;",
            [UpdatedUser, CronId]
        ),

        % 处理不同格式的返回值
        case Result of
            % {ok, RowCount} 格式
            {ok, _} ->
                ok;
            % {ok, Columns, Rows} 格式
            {ok, _, _} ->
                ok;
            % {ok, Count, Columns, Rows} 格式
            {ok, _, _, _} ->
                ok;
            Other ->
                lager:info("删除任务返回值: ~p", [Other]),
                ok
        end,

        {json, #{
            status => true,
            message => <<"任务删除成功！"/utf8>>,
            refresh => true
        }}
    catch
        ErrorType:ErrorReason:Stacktrace ->
            lager:error("任务删除失败：~p:~p~n~p~n", [ErrorType, ErrorReason, Stacktrace]),
            {json, #{
                status => false,
                message => <<"任务删除失败！"/utf8>>
            }}
    end;
delete(#{auth_data := #{<<"permission">> := #{<<"crontab">> := false}}}) ->
    {json, [#{<<"Alert">> => unicode:characters_to_binary("API鉴权失败！", utf8)}]};
delete(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 切换任务状态（启用/禁用）
%% @end
toggle(#{
    auth_data := #{
        <<"authed">> := true,
        <<"loginname">> := LoginName,
        <<"permission">> := #{<<"crontab">> := true}
    },
    params := #{<<"cronId">> := CronId}
}) ->
    try
        % 查询任务当前状态
        {ok, _, StatusData} = eadm_pgpool:equery(
            pool_pg,
            "select cronstatus from eadm_crontab where id = $1 and deleted is false;",
            [CronId]
        ),

        % 将数据库中的状态值转换为布尔值 (0表示启用，1表示禁用)
        CurrentStatus =
            case StatusData of
                % 当前是启用状态
                [{0}] -> true;
                % 当前是禁用状态
                [{1}] -> false;
                % 默认为禁用状态
                _ -> false
            end,

        % 切换状态（取反）
        NewStatus = not CurrentStatus,

        % 将布尔值转换为整数 (0表示启用，1表示禁用)
        StatusInt =
            case NewStatus of
                % 启用
                true -> 0;
                % 禁用
                false -> 1
            end,

        % 更新任务状态
        Result = eadm_pgpool:equery(
            pool_pg,
            "update eadm_crontab set cronstatus = $1, updateduser = $2, updatedat = now()
\n"
            "             where id = $3 returning cronexp, cronmfa, starttime, endtime;",
            [StatusInt, LoginName, CronId]
        ),

        % 解析查询结果
        ResData =
            case Result of
                {ok, _, _, Rows} -> Rows;
                {ok, _, Rows} -> Rows;
                _ -> []
            end,

        lager:info("任务状态更新结果: ~p", [Result]),
        lager:info("解析后的数据: ~p", [ResData]),

        case NewStatus of
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
                        % 先尝试删除可能存在的旧任务
                        JobId = list_to_atom("job_" ++ binary_to_list(CronId)),
                        try
                            ecron:delete(JobId)
                        catch
                            _:_ -> ok
                        end,
                        % 添加新任务
                        schedule_job(Job),
                        {json, #{
                            status => true,
                            message => <<"任务已激活"/utf8>>,
                            refresh => true
                        }};
                    _ ->
                        lager:info("无法解析任务数据: ~p", [ResData]),
                        {json, #{
                            status => false,
                            message => <<"任务激活失败，请确保任务类型为 Erlang"/utf8>>
                        }}
                end;
            false ->
                % 停用任务，从调度器中移除
                JobId = list_to_atom("job_" ++ binary_to_list(CronId)),
                try
                    ecron:delete(JobId)
                catch
                    _:_ -> ok
                end,
                {json, #{
                    status => true,
                    message => <<"任务已停用"/utf8>>,
                    refresh => true
                }}
        end
    catch
        ErrorType:ErrorReason:Stacktrace ->
            lager:error("任务状态切换失败：~p:~p~n~p~n", [ErrorType, ErrorReason, Stacktrace]),
            {json, #{
                status => false,
                message => <<"任务状态切换失败，请联系管理员"/utf8>>
            }}
    end;
toggle(#{auth_data := #{<<"permission">> := #{<<"crontab">> := false}}}) ->
    {json, [#{<<"Alert">> => unicode:characters_to_binary("API鉴权失败！", utf8)}]};
toggle(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%%====================================================================
%% 内部函数
%%====================================================================
