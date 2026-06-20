%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2024, REDGREAT
%%% @doc
%%%  Role service shared by Nova controllers and future Cowboy handlers.
%%% @end
%%%-------------------------------------------------------------------
-module(eadm_role_service).
-author("wangcw").

-export([list/0]).

%%====================================================================
%% API functions
%%====================================================================

list() ->
    {ok, ResCol, ResData} = eadm_pgpool:equery(pool_pg,
        "select id,
                rolename as \"roleName\",
                rolestatus as \"roleStatus\",
                createdat as \"createdAt\"
        from vi_role
        order by createdat;", []),
    Items = eadm_utils:pg_as_map(ResCol, ResData),
    #{
        <<"items">> => Items,
        <<"total">> => length(Items)
    }.
