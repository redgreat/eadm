%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2024, REDGREAT
%%% @doc
%%%  User service shared by Nova controllers and future Cowboy handlers.
%%% @end
%%%-------------------------------------------------------------------
-module(eadm_user_service).
-author("wangcw").

-export([list/0]).

%%====================================================================
%% API functions
%%====================================================================

list() ->
    {ok, ResCol, ResData} = eadm_pgpool:equery(pool_pg,
        "select id,
                tenantname as \"tenantName\",
                loginname as \"loginName\",
                username as \"userName\",
                email,
                userstatus as \"userStatus\",
                createdat as \"createdAt\"
        from vi_user
        order by createdat;", []),
    Items = eadm_utils:pg_as_map(ResCol, ResData),
    #{
        <<"items">> => Items,
        <<"total">> => length(Items)
    }.
