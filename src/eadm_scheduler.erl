%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2026, REDGREAT
%%% @doc
%%% 运动数据同步定时任务调度器
%%% 使用gen_server实现定时任务管理
%%% @end
%%% Created : 2026-01-23
%%%-------------------------------------------------------------------
-module(eadm_scheduler).
-author("wangcw").

-behaviour(gen_server).

%% API
-export([
    start_link/0,
    schedule_user_sync/1,
    trigger_manual_sync/2,
    cancel_user_sync/1,
    get_sync_status/1,
    schedule_job/4,
    delete_job/1,
    statistic/1,
    run_user_sync/1
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(SERVER, ?MODULE).
% 凌晨2点执行
-define(DAILY_SYNC_HOUR, 2).
% 30分钟重试间隔
-define(RETRY_INTERVAL, 1800000).

-record(state, {
    % UserId => TimerRef
    scheduled_tasks = #{} :: map(),
    % UserId => Pid
    running_syncs = #{} :: map()
}).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    application:ensure_all_started(ecron),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc 为用户调度定时同步任务
%%--------------------------------------------------------------------
-spec schedule_user_sync(UserId :: integer()) -> ok | {error, term()}.
schedule_user_sync(UserId) ->
    gen_server:call(?SERVER, {schedule_sync, UserId}).

%%--------------------------------------------------------------------
%% @doc 立即触发手动同步
%%--------------------------------------------------------------------
-spec trigger_manual_sync(UserId :: integer(), DaysBack :: integer()) ->
    {ok, pid()} | {error, term()}.
trigger_manual_sync(UserId, DaysBack) ->
    gen_server:call(?SERVER, {trigger_sync, UserId, DaysBack}).

%%--------------------------------------------------------------------
%% @doc 取消用户的定时同步
%%--------------------------------------------------------------------
-spec cancel_user_sync(UserId :: integer()) -> ok.
cancel_user_sync(UserId) ->
    gen_server:cast(?SERVER, {cancel_sync, UserId}).

%%--------------------------------------------------------------------
%% @doc 获取同步状态
%%--------------------------------------------------------------------
-spec get_sync_status(UserId :: integer()) ->
    {ok, running | scheduled | idle} | {error, term()}.
get_sync_status(UserId) ->
    gen_server:call(?SERVER, {get_status, UserId}).

schedule_job(JobName, Spec, MFA, Options) ->
    ecron:create(JobName, Spec, MFA, Options),
    ok.

delete_job(JobName) ->
    catch ecron:delete(JobName),
    ok.

statistic(JobName) ->
    ecron:statistic(JobName).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    schedule_all_auto_sync_users(),
    {ok, #state{}}.

handle_call({schedule_sync, UserId}, _From, State) ->
    {Reply, NewState} = do_schedule_sync(UserId, State),
    {reply, Reply, NewState};
handle_call({trigger_sync, UserId, DaysBack}, _From, State) ->
    {Reply, NewState} = do_trigger_sync(UserId, DaysBack, State),
    {reply, Reply, NewState};
handle_call({get_status, UserId}, _From, State) ->
    Status =
        case maps:is_key(UserId, State#state.running_syncs) of
            true ->
                running;
            false ->
                case maps:is_key(UserId, State#state.scheduled_tasks) of
                    true -> scheduled;
                    false -> idle
                end
        end,
    {reply, {ok, Status}, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({cancel_sync, UserId}, State) ->
    NewState = do_cancel_sync(UserId, State),
    {noreply, NewState};
handle_cast({sync_started, UserId}, State) ->
    NewRunning = maps:put(UserId, true, State#state.running_syncs),
    {noreply, State#state{running_syncs = NewRunning}};
handle_cast({sync_finished, UserId}, State) ->
    NewRunning = maps:remove(UserId, State#state.running_syncs),
    {noreply, State#state{running_syncs = NewRunning}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({sync_timer, UserId}, State) ->
    %% 定时器触发,执行同步
    NewState = do_execute_sync(UserId, State),
    %% 重新调度下一次同步
    {ok, State2} = do_schedule_sync(UserId, NewState),
    {noreply, State2};
handle_info({sync_complete, UserId, Result}, State) ->
    %% 同步任务完成
    logger:info("Sync complete for user ~p: ~p", [UserId, Result]),
    NewRunningSync = maps:remove(UserId, State#state.running_syncs),
    {noreply, State#state{running_syncs = NewRunningSync}};
handle_info({sync_failed, UserId, Reason}, State) ->
    %% 同步失败,记录日志并安排重试
    logger:error("Sync failed for user ~p: ~p", [UserId, Reason]),
    NewRunningSync = maps:remove(UserId, State#state.running_syncs),
    %% 30分钟后重试
    erlang:send_after(?RETRY_INTERVAL, self(), {sync_timer, UserId}),
    {noreply, State#state{running_syncs = NewRunningSync}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    %% 取消所有定时器
    maps:foreach(
        fun(_UserId, TimerRef) ->
            erlang:cancel_timer(TimerRef)
        end,
        State#state.scheduled_tasks
    ),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc 调度同步任务
%%--------------------------------------------------------------------
do_schedule_sync(UserId, State) ->
    State1 = do_cancel_sync(UserId, State),
    JobName = {sports_user_sync, UserId},
    Spec = "0 2 * * *",
    MFA = {?MODULE, run_user_sync, [UserId]},
    Options = #{singleton => true},
    _ = ecron:create(JobName, Spec, MFA, Options),
    NewTasks = maps:put(UserId, true, State1#state.scheduled_tasks),
    {ok, State1#state{scheduled_tasks = NewTasks}}.

%%--------------------------------------------------------------------
%% @private
%% @doc 触发立即同步
%%--------------------------------------------------------------------
do_trigger_sync(UserId, DaysBack, State) ->
    case maps:is_key(UserId, State#state.running_syncs) of
        true ->
            {{error, sync_already_running}, State};
        false ->
            Parent = self(),
            Pid = spawn_link(fun() ->
                sync_worker(UserId, DaysBack, Parent)
            end),

            NewRunningSync = maps:put(UserId, Pid, State#state.running_syncs),
            {{ok, Pid}, State#state{running_syncs = NewRunningSync}}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc 执行同步
%%--------------------------------------------------------------------
do_execute_sync(UserId, State) ->
    DaysBack = get_user_sync_days(UserId),
    _ = spawn_link(fun() -> sync_worker(UserId, DaysBack, self()) end),
    State.

%%--------------------------------------------------------------------
%% @private
%% @doc 取消同步任务
%%--------------------------------------------------------------------
do_cancel_sync(UserId, State) ->
    JobName = {sports_user_sync, UserId},
    catch ecron:delete(JobName),
    NewTasks = maps:remove(UserId, State#state.scheduled_tasks),
    State#state{scheduled_tasks = NewTasks}.

%%--------------------------------------------------------------------
%% @private
%% @doc 同步工作进程
%%--------------------------------------------------------------------
sync_worker(UserId, DaysBack, Parent) ->
    {ok, LogId} = log_sync_start(UserId),
    gen_server:cast(?SERVER, {sync_started, UserId}),

    try
        Result = garmin_sync_service:sync_user_activities(UserId, DaysBack),

        case Result of
            {ok, Stats} ->
                log_sync_complete(LogId, Stats),
                Parent ! {sync_complete, UserId, Stats};
            {error, Reason} ->
                log_sync_failed(LogId, Reason),
                Parent ! {sync_failed, UserId, Reason}
        end
    catch
        Error:Reason2:Stacktrace ->
            logger:error(
                "Sync worker crashed for user ~p: ~p:~p~n~p",
                [UserId, Error, Reason2, Stacktrace]
            ),
            log_sync_failed(LogId, {Error, Reason2}),
            Parent ! {sync_failed, UserId, {Error, Reason2}}
    end,
    gen_server:cast(?SERVER, {sync_finished, UserId}).

run_user_sync(UserId) ->
    gen_server:cast(?SERVER, {sync_started, UserId}),
    {ok, LogId} = log_sync_start(UserId),
    DaysBack = get_user_sync_days(UserId),
    try
        Result = garmin_sync_service:sync_user_activities(UserId, DaysBack),
        case Result of
            {ok, Stats} ->
                log_sync_complete(LogId, Stats),
                gen_server:cast(?SERVER, {sync_finished, UserId}),
                ok;
            {error, Reason} ->
                log_sync_failed(LogId, Reason),
                gen_server:cast(?SERVER, {sync_finished, UserId}),
                {error, Reason}
        end
    catch
        Error:Reason2:Stacktrace ->
            logger:error(
                "Sync worker crashed for user ~p: ~p:~p~n~p",
                [UserId, Error, Reason2, Stacktrace]
            ),
            log_sync_failed(LogId, {Error, Reason2}),
            gen_server:cast(?SERVER, {sync_finished, UserId}),
            {error, {Error, Reason2}}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc 计算下次运行时间（未使用，保留备用）
%%--------------------------------------------------------------------
calculate_next_run_time(_Hour) ->
    {{1970, 1, 1}, {0, 0, 0}}.

%%--------------------------------------------------------------------
%% @private
%% @doc 加载所有自动同步用户
%%--------------------------------------------------------------------
schedule_all_auto_sync_users() ->
    SQL =
        <<
            "SELECT userid FROM sp_garminconf \n"
            "\n"
            "            WHERE syncenable = true AND autosync = true"
        >>,

    case eadm_pgpool:equery(SQL, []) of
        {ok, _, Rows} ->
            lists:foreach(
                fun({UserId}) ->
                    schedule_user_sync(UserId)
                end,
                Rows
            ),
            ok;
        {error, Reason} ->
            logger:error("Failed to load auto sync users: ~p", [Reason]),
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc 获取用户配置的同步天数
%%--------------------------------------------------------------------
get_user_sync_days(UserId) ->
    SQL = <<"SELECT syncdays FROM sp_garminconf WHERE userid = $1">>,

    case eadm_pgpool:equery(SQL, [UserId]) of
        {ok, _, [{Days}]} ->
            Days;
        _ ->
            % 默认30天
            30
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc 记录同步开始
%%--------------------------------------------------------------------
log_sync_start(UserId) ->
    SQL =
        <<
            "INSERT INTO sp_garminlog \n"
            "\n"
            "            (userid, starttime, syncstatus)\n"
            "\n"
            "            VALUES ($1, CURRENT_TIMESTAMP, 'running')\n"
            "\n"
            "            RETURNING id"
        >>,

    case eadm_pgpool:equery(SQL, [UserId]) of
        {ok, _, [{LogId}]} ->
            {ok, LogId};
        Error ->
            logger:error("Failed to log sync start: ~p", [Error]),
            {ok, 0}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc 记录同步完成
%%--------------------------------------------------------------------
log_sync_complete(LogId, Stats) ->
    SQL =
        <<
            "UPDATE sp_garminlog \n"
            "\n"
            "            SET endtime = CURRENT_TIMESTAMP,\n"
            "\n"
            "                synccount = $1,\n"
            "\n"
            "                newcount = $2,\n"
            "\n"
            "                syncstatus = 'success'\n"
            "\n"
            "            WHERE id = $3"
        >>,

    eadm_pgpool:equery(SQL, [
        maps:get(synced, Stats, 0),
        maps:get(new, Stats, 0),
        LogId
    ]).

%%--------------------------------------------------------------------
%%% @private
%% @doc 记录同步失败
%%--------------------------------------------------------------------
log_sync_failed(LogId, Reason) ->
    SQL =
        <<
            "UPDATE sp_garminlog \n"
            "\n"
            "            SET endtime = CURRENT_TIMESTAMP,\n"
            "\n"
            "                syncstatus = 'failed',\n"
            "\n"
            "                errmsg = $1\n"
            "\n"
            "            WHERE id = $2"
        >>,

    ErrorMsg = io_lib:format("~p", [Reason]),
    eadm_pgpool:equery(SQL, [list_to_binary(ErrorMsg), LogId]).
