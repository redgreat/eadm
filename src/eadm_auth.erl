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

%% API
-export([auth/1]).

%%%===================================================================
%%% default check auth Callback
%%%===================================================================
auth(Req) ->
    case nova_session:get(Req ,<<"exp">>) of
        {ok, Exp} ->
            lager:info("Auth Exp:~p~n", [Exp]),
            case is_integer(Exp) andalso (Exp > erlang:system_time(seconds)) of
                true ->
                    {ok, Username} = nova_session:get(Req ,<<"username">>),
                    NewExp = eadm_utils:get_exp_bin(),
                    nova_session:set(Req, <<"exp">>, NewExp),
                    lager:info("User: ~p Auth Success! Exp: ~p, NewExp: ~p~n", [Username, Exp, NewExp]),
                    {true, #{<<"authed">> => true, <<"username">> => Username}};
                false ->
                    lager:info("Auth Failed, Exp: ~p Expired!", [Exp]),
                    {true, #{<<"authed">> => false}}
            end;
        {error, _SessionErr} ->
            lager:info("Auth Failed, SessionError!~n"),
            {true, #{<<"authed">> => false}}
    end.

%%%===================================================================
%%% 内部函数
%%%===================================================================
