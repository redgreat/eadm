%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2024, REDGREAT
%%% @doc
%%% Worker for poolboy.  Initial code from
%%% https://github.com/devinus/poolboy
%%% @end
%%% Created : 2024-06-17 上午11:36
%%%-------------------------------------------------------------------
-module(eadm_pgpool_worker).
-author("wangcw").

%%%===================================================================
%%% 行为
%%%===================================================================
-behaviour(gen_server).
-behaviour(poolboy_worker).

%%%===================================================================
%%% 函数导出
%%%===================================================================
-export([squery/1, squery/2, squery/3,
         equery/2, equery/3, equery/4,
         with_transaction/2, with_transaction/3]).

-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%%===================================================================
%%% 宏定义
%%%===================================================================
-record(state, {conn::pid(),
                delay::pos_integer(),
                timer::timer:tref(),
                start_args::proplists:proplist()}).

%%%===================================================================
%%% 宏定义
%%%===================================================================
-define(INITIAL_DELAY, 500). % Half a second
-define(MAXIMUM_DELAY, 5 * 60 * 1000). % Five minutes
-define(TIMEOUT, 5 * 1000).

-define(STATE_VAR, '$pgapp_state').

%%====================================================================
%% API 函数
%%====================================================================

%% @doc
%% 脚本执行
%% @end
squery(Sql) ->
    case get(?STATE_VAR) of
        undefined ->
            squery(pool_pg, Sql);
        Conn ->
            epgsql:squery(Conn, Sql)
    end.

squery(PoolName, Sql) when is_atom(PoolName) ->
    squery(PoolName, Sql, ?TIMEOUT);
squery(Sql, Timeout) ->
    squery(epgsql_pool, Sql, Timeout).

squery(PoolName, Sql, Timeout) ->
    middle_man_transaction(PoolName,
                           fun (W) ->
                                   gen_server:call(W, {squery, Sql}, Timeout)
                           end, Timeout).

%% @doc
%% 带参数脚本执行
%% @end
equery(Sql, Params) ->
    case get(?STATE_VAR) of
        undefined ->
            equery(epgsql_pool, Sql, Params);
        Conn ->
            epgsql:equery(Conn, Sql, Params)
    end.

equery(PoolName, Sql, Params) when is_atom(PoolName) ->
    equery(PoolName, Sql, Params, ?TIMEOUT);
equery(Sql, Params, Timeout) ->
    equery(epgsql_pool, Sql, Params, Timeout).

equery(PoolName, Sql, Params, Timeout) ->
    middle_man_transaction(PoolName,
                           fun (W) ->
                                   gen_server:call(W, {equery, Sql, Params},
                                                   Timeout)
                           end, Timeout).

%% @doc
%% 开启事务
%% @end
with_transaction(PoolName, Fun) ->
    with_transaction(PoolName, Fun, ?TIMEOUT).

with_transaction(PoolName, Fun, Timeout) ->
    middle_man_transaction(PoolName,
                           fun (W) ->
                                   gen_server:call(W, {transaction, Fun},
                                                   Timeout)
                           end, Timeout).

%% @doc
%% 开启事务
%% @end
middle_man_transaction(Pool, Fun, Timeout) ->
    Tag = make_ref(),
    {Receiver, Ref} = erlang:spawn_monitor(
                        fun() ->
                                process_flag(trap_exit, true),
                                Result = poolboy:transaction(Pool, Fun,
                                                             Timeout),
                                exit({self(),Tag,Result})
                        end),
    receive
        {'DOWN', Ref, _, _, {Receiver, Tag, Result}} ->
            Result;
        {'DOWN', Ref, _, _, {timeout, _}} ->
            {error, timeout};
        {'DOWN', Ref, _, _, Reason} ->
            {error, Reason}
    end.

%% @doc
%% 开启进程
%% @end
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

%% @doc
%% 初始化
%% @end
init(Args) ->
    process_flag(trap_exit, true),
    {ok, connect(#state{start_args = Args, delay = ?INITIAL_DELAY})}.

%% @doc
%% 提交处理逻辑
%% @end
handle_call(_Query, _From, #state{conn = undefined} = State) ->
    {reply, {error, disconnected}, State};
handle_call({squery, Sql}, _From,
            #state{conn=Conn} = State) ->
    {reply, epgsql:squery(Conn, Sql), State};
handle_call({equery, Sql, Params}, _From,
            #state{conn = Conn} = State) ->
    {reply, epgsql:equery(Conn, Sql, Params), State};
handle_call({transaction, Fun}, _From,
            #state{conn = Conn} = State) ->
    put(?STATE_VAR, Conn),
    Result = epgsql:with_transaction(Conn, fun(_) -> Fun() end),
    erase(?STATE_VAR),
    {reply, Result, State}.

%% @doc
%% 处理异步消息，用于重新连接数据库
%% @end
handle_cast(reconnect, State) ->
    {noreply, connect(State)}.

%% @doc
%% 处理进程退出消息，当数据库连接断开时尝试重新连接
%% @end
handle_info({'EXIT', From, Reason}, State) ->
    {NewDelay, Tref} =
        case State#state.timer of
            undefined ->
                %% We add a timer here only if there's not one that's
                %% already active.
                Delay = calculate_delay(State#state.delay),
                {ok, T} =
                    timer:apply_after(
                      State#state.delay,
                      gen_server, cast, [self(), reconnect]),
                {Delay, T};
            Timer ->
                {State#state.delay, Timer}
        end,

    error_logger:warning_msg(
      "~p EXIT from ~p: ~p - attempting to reconnect in ~p ms~n",
      [self(), From, Reason, NewDelay]),
    {noreply, State#state{conn = undefined, delay = NewDelay, timer = Tref}}.

%% @doc
%% 进程终止时的清理工作，关闭数据库连接
%% @end
terminate(_Reason, #state{conn = undefined}) ->
    ok;
terminate(_Reason, #state{conn = Conn}) ->
    ok = epgsql:close(Conn),
    ok.

%% @doc
%% 代码热更新时的状态转换函数
%% @end
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% @doc
%% 连接数据库函数，处理连接成功和失败的情况
%% @end
connect(State) ->
    Args = State#state.start_args,
    Hostname = proplists:get_value(host, Args),
    Database = proplists:get_value(database, Args),
    Username = proplists:get_value(username, Args),

    case epgsql:connect(Args) of
        {ok, Conn} ->
            error_logger:info_msg(
              "~p Connected to ~s at ~s with user ~s: ~p~n",
              [self(), Database, Hostname, Username, Conn]),
            timer:cancel(State#state.timer),
            State#state{conn=Conn, delay=?INITIAL_DELAY, timer = undefined};
        Error ->
            NewDelay = calculate_delay(State#state.delay),
            error_logger:warning_msg(
              "~p Unable to connect to ~s at ~s with user ~s (~p) "
              "- attempting reconnect in ~p ms~n",
              [self(), Database, Hostname, Username, Error, NewDelay]),
            {ok, Tref} =
                timer:apply_after(
                  State#state.delay, gen_server, cast, [self(), reconnect]),
            State#state{conn=undefined, delay = NewDelay, timer = Tref}
    end.

%% @doc
%% 计算重连延迟时间，采用指数退避策略，但不超过最大延迟时间
%% @end
calculate_delay(Delay) when (Delay * 2) >= ?MAXIMUM_DELAY ->
    ?MAXIMUM_DELAY;
calculate_delay(Delay) ->
    Delay * 2.

%%====================================================================
%% 内部函数
%%====================================================================
