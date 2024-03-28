%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2024, REDGREAT
%%% @doc
%%%
%%% @end
%%% Created : 2024-03-19 09:20
%%%-------------------------------------------------------------------

%%%-------------------------------------------------------------------
%% @doc s public API
%% 编译并热更新
%% 引用自 https://gitee.com/tercero/erlutils/blob/master/src/s.erl
%% @end
%%%-------------------------------------------------------------------

-module(s).
-author("wangcw").

-include_lib("kernel/include/logger.hrl").

-export([ s/0
        , l/0
        , l/1
        , r/0
        ]).


%% 研发中使用，先用 rebar3 编译，然后重新载入
s() ->
    % [?LOG_NOTICE(Info) || Info <- string:replace( os:cmd("rebar3 compile"), "\n", "", all ), Info =/= []],
    lager:info(string:replace( os:cmd("rebar3 compile"), "\n", "", all )),
    r().

%% 重新载入默认 app
l() ->
    [l(App) || {App, _Description, _Vsn} <- application:loaded_applications()].

%% 重新载入，需要指定 app
l(LoadApps) when is_list(LoadApps) ->
    F = fun(App, List) ->
        {ok, MS} = application:get_key(App, modules),
        List ++ MS
    end,
    Modules = lists:foldl(F, [], LoadApps),
    update(Modules),
    ok;
l(LoadApp) -> l([LoadApp]).


%% 重新载入所有已经载入过的模块
r() ->
    Modules = erlang:loaded(),
    update(Modules),
    ok.

%% internal functions
update(Modules) ->
    update_loop(Modules, [], []).

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
    ?LOG_NOTICE("nothing updated!!!");
update_loop([], Succ, []) ->
    ?LOG_NOTICE("succ: ~p", [Succ]);
update_loop([], [], Fail) ->
    ?LOG_NOTICE("fail: ~p", [Fail]);
update_loop([], Succ, Fail) ->
    ?LOG_NOTICE("succ: ~p", [Succ]),
    ?LOG_NOTICE("fail: ~p", [Fail]).

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

soft_update(Module) ->
    case code:soft_purge(Module) of
        true ->
            code:load_file(Module);
        false ->
            {error, "not purge"}
    end.

