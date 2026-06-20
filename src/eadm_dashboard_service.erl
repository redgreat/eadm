%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2024, REDGREAT
%%% @doc
%%%  Dashboard data service shared by Nova controllers and future Cowboy handlers.
%%% @end
%%%-------------------------------------------------------------------
-module(eadm_dashboard_service).
-author("wangcw").

-export([summary/1]).

%%====================================================================
%% API functions
%%====================================================================

summary(LoginName) ->
    {ok, _, ResData} = eadm_pgpool:equery(pool_pg,
        "with dt as (
            select unnest(array[1,2,3,4]) as datatype
        )
        select coalesce(d.datavalue, '0')
        from dt
        left join eadm_dashboard d
            on d.datatype = dt.datatype
            and d.loginname = $1
            and d.datavalue is not null
        order by dt.datatype;", [LoginName]),
    {ok, _, ResLocation} = eadm_pgpool:equery(pool_pg,
        "select cast(right(checkdate, 2) as int) as month, datavalue
        from eadm_dashboard
        where loginname = $1
            and datatype = 5
        order by cast(right(checkdate, 2) as int);", [LoginName]),
    {ok, _, ResFinanceIn} = eadm_pgpool:equery(pool_pg,
        "select cast(right(checkdate, 2) as int) as month, datavalue
        from eadm_dashboard
        where loginname = $1
            and datatype = 6
        order by cast(right(checkdate, 2) as int);", [LoginName]),
    {ok, _, ResFinanceOut} = eadm_pgpool:equery(pool_pg,
        "select cast(right(checkdate, 2) as int) as month, datavalue
        from eadm_dashboard
        where loginname = $1
            and datatype = 7
        order by cast(right(checkdate, 2) as int);", [LoginName]),
    DataValues = [V || {V} <- ResData],
    #{
        <<"cards">> => #{
            <<"health">> => at(DataValues, 1),
            <<"location">> => at(DataValues, 2),
            <<"financeIncome">> => at(DataValues, 3),
            <<"financeExpense">> => at(DataValues, 4)
        },
        <<"locationTrend">> => #{
            <<"labels">> => labels(ResLocation),
            <<"values">> => values(ResLocation)
        },
        <<"financeTrend">> => #{
            <<"labels">> => labels(ResFinanceIn),
            <<"income">> => values(ResFinanceIn),
            <<"expense">> => values(ResFinanceOut)
        }
    }.

%%====================================================================
%% Internal functions
%%====================================================================

at(List, Index) ->
    case length(List) >= Index of
        true -> lists:nth(Index, List);
        false -> <<"0">>
    end.

labels(List) ->
    Mon = unicode:characters_to_binary("月", utf8),
    [month_label(Month, Mon) || {Month, _Value} <- List].

values(List) ->
    [Value || {_Month, Value} <- List].

month_label(Month, Mon) when is_integer(Month) ->
    MonthBin = integer_to_binary(Month),
    <<MonthBin/binary, Mon/binary>>;
month_label(Month, Mon) when is_binary(Month) ->
    <<Month/binary, Mon/binary>>;
month_label(Month, Mon) ->
    MonthBin = unicode:characters_to_binary(io_lib:format("~p", [Month]), utf8),
    <<MonthBin/binary, Mon/binary>>.
