-module(test_json_comparison).
-export([compare_json_libs/0]).

compare_json_libs() ->
    TestData = #{
        <<"name">> => <<"张三">>,
        <<"age">> => 30,
        <<"active">> => true,
        <<"scores">> => [85, 90, 78],
        <<"profile">> => #{
            <<"email">> => <<"zhangsan@example.com">>,
            <<"phone">> => <<"13800138000">>
        }
    },
    
    % 测试jsx编码
    JsxResult = try
        jsx:encode(TestData)
    catch 
        _:Error -> {jsx_error, Error}
    end,
    
    % 测试thoas编码
    ThoasResult = try
        thoas:encode(TestData)
    catch 
        _:Error -> {thoas_error, Error}
    end,
    
    % 测试内置json编码
    JsonResult = try
        json:encode(TestData)
    catch 
        _:Error -> {json_error, Error}
    end,
    
    % 输出结果比较
    io:format("=== JSON Library Comparison ===~n"),
    io:format("JSX: ~p~n", [JsxResult]),
    io:format("Thoas: ~p~n", [ThoasResult]),
    io:format("JSON: ~p~n", [JsonResult]),
    
    % 测试解码
    case JsxResult of
        {jsx_error, _} -> ok;
        _ ->
            JsxDecoded = jsx:decode(JsxResult, [return_maps]),
            io:format("JSX Decoded: ~p~n", [JsxDecoded])
    end,
    
    case ThoasResult of
        {thoas_error, _} -> ok;
        _ ->
            {ok, ThoasDecoded} = thoas:decode(ThoasResult),
            io:format("Thoas Decoded: ~p~n", [ThoasDecoded])
    end,
    
    case JsonResult of
        {json_error, _} -> ok;
        _ ->
            {ok, JsonDecoded} = json:decode(JsonResult),
            io:format("JSON Decoded: ~p~n", [JsonDecoded])
    end.
