-module(eadm_api_response_tests).

-include_lib("eunit/include/eunit.hrl").

ok_response_test() ->
    Response = eadm_api_response:ok(#{<<"id">> => 1}, done),
    ?assertEqual(true, maps:get(<<"success">>, Response)),
    ?assertEqual(<<"ok">>, maps:get(<<"code">>, Response)),
    ?assertEqual(<<"done">>, maps:get(<<"message">>, Response)),
    ?assertEqual(#{<<"id">> => 1}, maps:get(<<"data">>, Response)).

error_response_test() ->
    Response = eadm_api_response:error(validation_error, "bad", #{field => login_name}),
    ?assertEqual(false, maps:get(<<"success">>, Response)),
    ?assertEqual(<<"validation_error">>, maps:get(<<"code">>, Response)),
    ?assertEqual(<<"bad">>, maps:get(<<"message">>, Response)).

common_errors_test() ->
    ?assertMatch(#{<<"code">> := <<"unauthorized">>}, eadm_api_response:unauthorized()),
    ?assertMatch(#{<<"code">> := <<"forbidden">>}, eadm_api_response:forbidden()),
    ?assertMatch(#{<<"code">> := <<"not_found">>}, eadm_api_response:not_found()).
