%% 简单的JSON库格式比较测试
-module(json_test).
-export([test/0]).

test() ->
    Data = #{<<"test">> => <<"value">>, <<"number">> => 42},
    
    % 如果jsx可用
    JsxResult = case code:is_loaded(jsx) of
        {file, _} -> jsx:encode(Data);
        _ -> <<"jsx_not_available">>
    end,
    
    % 如果thoas可用  
    ThoasResult = case code:is_loaded(thoas) of
        {file, _} -> thoas:encode(Data);
        _ -> <<"thoas_not_available">>
    end,
    
    % 内置json模块 (Erlang 26+)
    JsonResult = case code:is_loaded(json) of
        {file, _} -> json:encode(Data);
        _ -> <<"json_not_available">>
    end,
    
    io:format("JSX: ~p~n", [JsxResult]),
    io:format("Thoas: ~p~n", [ThoasResult]), 
    io:format("JSON: ~p~n", [JsonResult]).
