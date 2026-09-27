-module(eadm_cowboy_session_tests).

-include_lib("eunit/include/eunit.hrl").

session_round_trip_test() ->
    application:set_env(eadm, secret_key, <<"unit-test-secret">>),
    Data = #{<<"loginName">> => <<"admin">>, <<"permission">> => #{<<"dashboard">> => true}},
    Token = eadm_cowboy_session:sign(Data),
    ?assertEqual({ok, Data}, eadm_cowboy_session:verify(Token)).

tampered_session_test() ->
    application:set_env(eadm, secret_key, <<"unit-test-secret">>),
    Token = eadm_cowboy_session:sign(#{<<"loginName">> => <<"admin">>}),
    ?assertEqual({error, invalid_signature}, eadm_cowboy_session:verify(<<Token/binary, "x">>)).

invalid_session_test() ->
    ?assertEqual({error, invalid_token}, eadm_cowboy_session:verify(<<"invalid">>)),
    ?assertEqual({error, invalid_token}, eadm_cowboy_session:verify(undefined)).
