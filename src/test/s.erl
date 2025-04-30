%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2024, REDGREAT
%%% @doc
%%% 编译并热更新
%% 引用自 https://gitee.com/tercero/erlutils/blob/master/src/s.erl
%%% @end
%%% Created : 2024-03-19 09:20
%%%-------------------------------------------------------------------
-module(s).
-author("wangcw").

%%%===================================================================
%%% 函数导出
%%%===================================================================
-export([ s/0, l/0, l/1, r/0]).

%%====================================================================
%% API 函数
%%====================================================================
%% @doc
%% 研发中使用，先用 rebar3 编译，然后重新载入模块
%% @end
s() ->
    % [?LOG_NOTICE(Info) || Info <- string:replace( os:cmd("rebar3 compile"), "\n", "", all ), Info =/= []],
    lager:info(string:replace( os:cmd("rebar3 compile"), "\n", "", all )),
    r().

%% @doc
%% 重新载入所有已加载的应用模块
%% @end
l() ->
    [l(App) || {App, _Description, _Vsn} <- application:loaded_applications()].

%% @doc
%% 重新载入指定应用的模块
%% @end
l(LoadApps) when is_list(LoadApps) ->
    F = fun(App, List) ->
        {ok, MS} = application:get_key(App, modules),
        List ++ MS
    end,
    Modules = lists:foldl(F, [], LoadApps),
    update(Modules),
    ok;
l(LoadApp) -> l([LoadApp]).


%% @doc
%% 重新载入所有已经载入过的模块
%% @end
r() ->
    Modules = erlang:loaded(),
    update(Modules),
    ok.

%%====================================================================
%% 内部函数
%%====================================================================

%% @private
%% @doc
%% 更新指定的模块列表
%% @end
update(Modules) ->
    update_loop(Modules, [], []).

%% @private
%% @doc
%% 递归更新模块，并记录成功和失败的结果
%% @end
update_loop([Module | Modules], Succ, Fail) ->
    case do_update(Module) of
        ignore ->
            update_loop(Modules, Succ, Fail);
        {error, Info} ->
            update_loop(Modules, Succ, [{Module, Info} | Fail]);
        {module, Module} ->
            update_loop(Modules, [Module | Succ], Fail)
    end;
update_loop([], [], []) ->
    lager:notice("nothing updated!!!");
update_loop([], Succ, []) ->
    lager:notice("succ: ~p", [Succ]);
update_loop([], [], Fail) ->
    lager:notice("fail: ~p", [Fail]);
update_loop([], Succ, Fail) ->
    lager:notice("succ: ~p", [Succ]),
    lager:notice("fail: ~p", [Fail]).

%% @private
%% @doc
%% 执行模块更新，根据模块状态决定是否需要更新
%% @end
do_update(Module) ->
    case code:module_status(Module) of
        modified ->
            soft_update(Module);
        not_loaded ->
            ignore;
        loaded ->
            ignore;
        removed ->
            {error, "file removed"}
    end.

%% @private
%% @doc
%% 软更新模块，先尝试清除模块，然后重新加载
%% @end
soft_update(Module) ->
    case code:soft_purge(Module) of
        true ->
            code:load_file(Module);
        false ->
            {error, "not purge"}
    end.
