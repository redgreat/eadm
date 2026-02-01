%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2024, REDGREAT
%%% @doc
%%%
%%% @end
%%% Created : 2024-01-26 14:40
%%%-------------------------------------------------------------------
-module(eadm_auth).
-author("wangcw").

%%%===================================================================
%%% 导出函数
%%%===================================================================
-export([auth/1]).

%%%===================================================================
%%% API 函数
%%%===================================================================

%% @doc
%% 主函数
%% @end
auth(Req) ->
    Path = cowboy_req:path(Req),
    case nova_session:get(Req, <<"exp">>) of
        {ok, Exp} ->
            case erlang:is_integer(Exp) andalso (Exp > erlang:system_time(seconds)) of
                true ->
                    {ok, LoginName} = nova_session:get(Req, <<"loginname">>),
                    {ok, UserName} = nova_session:get(Req, <<"username">>),
                    {ok, Permission} = nova_session:get(Req, <<"permission">>),
                    % lager:debug("Auth Success! User: ~ts, Path: ~ts", [UserName, Path]),
                    NewExp = eadm_utils:get_exp_bin(),
                    nova_session:set(Req, <<"exp">>, NewExp),
                    {true, #{
                        <<"authed">> => true,
                        <<"username">> => UserName,
                        <<"loginname">> => LoginName,
                        <<"permission">> => Permission
                    }};
                false ->
                    lager:debug("Session expired for path: ~ts", [Path]),
                    {true, #{<<"authed">> => false}}
            end;
        {error, _SessionErr} ->
            lager:debug("No session found for path: ~ts", [Path]),
            {true, #{<<"authed">> => false}}
    end.

%%%===================================================================
%%% 内部函数
%%%===================================================================
