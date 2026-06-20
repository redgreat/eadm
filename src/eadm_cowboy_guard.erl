%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2024, REDGREAT
%%% @doc
%%%  Session and permission helpers for native Cowboy API handlers.
%%% @end
%%%-------------------------------------------------------------------
-module(eadm_cowboy_guard).
-author("wangcw").

-export([allow_internal_or_require/2, current_user/1, require/2]).

%%====================================================================
%% API functions
%%====================================================================

current_user(Req) ->
    Cookies = cowboy_req:parse_cookies(Req),
    case lists:keyfind(<<"eadm_session">>, 1, Cookies) of
        {_, Token} -> eadm_cowboy_session:verify(Token);
        false -> {error, unauthorized}
    end.

allow_internal_or_require(Req, PermissionSpec) ->
    case cowboy_req:path(Req) of
        <<"/api/internal/", _Rest/binary>> -> {ok, internal};
        _ -> require(Req, PermissionSpec)
    end.

require(Req, PermissionSpec) ->
    case current_user(Req) of
        {ok, User} ->
            Permission = maps:get(<<"permission">>, User, #{}),
            case has_permission(Permission, PermissionSpec) of
                true -> {ok, User};
                false -> {error, forbidden}
            end;
        Error ->
            Error
    end.

%%====================================================================
%% Internal functions
%%====================================================================

has_permission(_Permission, any) ->
    true;
has_permission(Permission, Key) when is_binary(Key) ->
    maps:get(Key, Permission, false) =:= true;
has_permission(Permission, [Key]) ->
    has_permission(Permission, Key);
has_permission(Permission, [Key | Rest]) ->
    case maps:get(Key, Permission, undefined) of
        SubPermission when is_map(SubPermission) -> has_permission(SubPermission, Rest);
        _ -> false
    end.
