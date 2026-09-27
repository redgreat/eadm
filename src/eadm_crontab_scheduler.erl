%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2024, REDGREAT
%%% @doc
%%%
%%% 定时任务调度初始化
%%%
%%% @end
%%% Created : 2024-04-02 19:48:17
%%%-------------------------------------------------------------------
-module(eadm_crontab_scheduler).
-author("wangcw").

%%%===================================================================
%%% 函数导出
%%%===================================================================
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
load_and_schedule_jobs() ->

    try
        % 从数据库加载所有激活的定时任务
        case eadm_pgpool:equery(pool_pg,
            "select id, cronname, cronexp, cronmfa, starttime, endtime
             from eadm_crontab
             where cronstatus = 0
              and deleted is false;", []) of
            {ok, Columns, ResData} ->
                JsonResult = eadm_utils:pg_as_json(Columns, ResData),
                case JsonResult of
                    #{data := Jobs} when is_list(Jobs), length(Jobs) > 0 ->
                        lager:info("找到 ~p 个需要初始化的定时任务", [length(Jobs)]),
                        lists:foreach(fun(Job) ->
                            try
                                CronName = maps:get(<<"cronname">>, Job, <<"未知任务">>),
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
                                    lager:error("初始化任务失败: ~p:~p~n~p~n任务数据: ~p",
                                               [InitErrorType, InitErrorReason, InitStacktrace, Job])
                            end
                        end, Jobs);
                    #{data := []} ->
                        lager:info("没有找到需要初始化的定时任务");
                    _ ->
                        lager:error("解析任务数据失败: ~p", [JsonResult])
                end;
            {error, Error} ->
                lager:error("查询定时任务失败: ~p", [Error]);
            Other ->
                lager:error("查询定时任务返回未知结果: ~p", [Other])
        end
    catch
        ErrorType:ErrorReason:Stacktrace ->
            lager:error("从数据库加载任务失败: ~p:~p~n~p", [ErrorType, ErrorReason, Stacktrace])
    end.

%% @doc
%% 调度任务函数
%% @end
schedule_job(#{<<"id">> := Id, <<"cronexp">> := CronExp, <<"cronmfa">> := CronMFA} = Job) ->
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

                lager:info("解析MFA: ~p:~p/~p -> ~p:~p(~p)",
                          [ModStr, FunStr, ArgsStr, Mod, Fun, Args]),

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
                                            lager:error("任务执行失败: ~p:~p~n~p", [ErrorType, ErrorReason, Stacktrace])
                                    end
                                end,

                                case code:ensure_loaded(ecron) of
                                    {module, _} ->
                                        ParsedStartTime = case StartTime of
                                            undefined -> unlimited;
                                            null -> unlimited;
                                            {{_,_,_},{H1,M1,S1}} -> {H1,M1,S1};
                                            _ -> unlimited
                                        end,
                                        ParsedEndTime = case EndTime of
                                            undefined -> unlimited;
                                            null -> unlimited;
                                            {{_,_,_},{H2,M2,S2}} -> {H2,M2,S2};
                                            _ -> unlimited
                                        end,
                                        Result = ecron:add(ecron_local, JobId, binary_to_list(CronExp), {erlang, apply, [JobFun, []]},
                                                          ParsedStartTime, ParsedEndTime, [{singleton, true}]),
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
            lager:error("调度任务失败: ~p:~p~n~p~n任务数据: ~p",
                       [ErrorType, ErrorReason, Stacktrace, Job]),
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
                    false -> [Args]  % 如果不是列表，将其包装成列表
                end
        end
    catch
        ErrorType:ErrorReason:Stacktrace ->
            lager:info("解析参数失败: ~p:~p~n~p~n参数字符串: ~p",
                      [ErrorType, ErrorReason, Stacktrace, ArgsStr]),
            % 返回空列表作为默认值
            []
    end.

