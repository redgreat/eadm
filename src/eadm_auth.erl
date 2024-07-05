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
    case nova_session:get(Req ,<<"exp">>) of
        {ok, Exp} ->
            case is_integer(Exp) andalso (Exp > erlang:system_time(seconds)) of
                true ->
                    {ok, LoginName} = nova_session:get(Req ,<<"loginname">>),
                    {ok, UserName} = nova_session:get(Req ,<<"username">>),
                    {ok, Permission} = nova_session:get(Req ,<<"permission">>),
                    NewExp = eadm_utils:get_exp_bin(),
                    nova_session:set(Req, <<"exp">>, NewExp),
                    % lager:info("User: ~ts ~p Auth Success! Exp: ~p, NewExp: ~p", [UserName, self(), Exp, NewExp]),
                    {true, #{<<"authed">> => true, <<"username">> => UserName,
                        <<"loginname">> => LoginName, <<"permission">> => Permission}};
                false ->
                    lager:info("Auth Failed, Exp: ~p Expired!", [Exp]),
                    {true, #{<<"authed">> => false}}
            end;
        {error, _SessionErr} ->
            lager:info("Auth Failed, SessionError!"),
            {true, #{<<"authed">> => false}}
    end.

%%%===================================================================
%%% 内部函数
%%%===================================================================
