%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2024, REDGREAT
%%% @doc
%%%  Crontab service shared by Nova controllers and future Cowboy handlers.
%%% @end
%%%-------------------------------------------------------------------
-module(eadm_crontab_service).
-author("wangcw").

-export([list/1]).

%%====================================================================
%% API functions
%%====================================================================

list(CronName) ->
    {ok, ResCol, ResData} = eadm_pgpool:equery(pool_pg,
        "select id,
                cronname as \"cronName\",
                cronexp as \"cronExp\",
                cronmfa as \"cronMfa\",
                starttime as \"startTime\",
                endtime as \"endTime\",
                cronstatus as \"cronStatus\",
                createdat as \"createdAt\"
        from vi_crontab
        where cronname like $1
        order by createdat desc;", [<<"%", CronName/binary, "%">>]),
    Items = eadm_utils:pg_as_map(ResCol, ResData),
    #{
        <<"items">> => Items,
        <<"total">> => length(Items)
    }.
