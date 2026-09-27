-module(eadm_geo_tests).

-include_lib("eunit/include/eunit.hrl").

outside_china_is_unchanged_test() ->
    ?assertEqual({-0.1276, 51.5072}, eadm_geo:wgs84_to_gcj02({-0.1276, 51.5072})).

beijing_is_transformed_test() ->
    {Lng, Lat} = eadm_geo:wgs84_to_gcj02({116.397128, 39.916527}),
    ?assert(Lng > 116.40),
    ?assert(Lng < 116.41),
    ?assert(Lat > 39.91),
    ?assert(Lat < 39.93).
