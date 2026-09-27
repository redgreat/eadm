-module(eadm_http_smoke_tests).

-include_lib("eunit/include/eunit.hrl").

ping_endpoint_test_() ->
    {timeout, 30, {setup, fun setup/0, fun cleanup/1,
        fun(State) -> [?_test(ping(State))] end}}.

setup() ->
    application:set_env(eadm, cowboy_port, 0),
    {ok, Pid} = eadm_cowboy_http:start_link(),
    unlink(Pid),
    {Pid, ranch:get_port(eadm_cowboy_http)}.

cleanup({Pid, _Port}) ->
    gen_server:stop(Pid).

ping({_Pid, Port}) ->
    {ok, {{_Version, 200, _Reason}, Headers, Body}} =
        httpc:request(get, {"http://127.0.0.1:" ++ integer_to_list(Port) ++ "/api/v1/ping", []}, [], []),
    ?assertEqual("application/json; charset=utf-8", proplists:get_value("content-type", Headers)),
    {ok, Json} = thoas:decode(list_to_binary(Body)),
    ?assertEqual(true, maps:get(<<"success">>, Json)),
    ?assertMatch(#{<<"service">> := <<"eadm">>}, maps:get(<<"data">>, Json)).
