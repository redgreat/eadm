#!/usr/bin/env escript
%%% -*- erlang -*-
%%% @doc
%%% 配置文件加载测试脚本
%%% 用于验证 db.config 文件是否能正确加载
%%% @end

main([]) ->
    io:format("~n=== 开始测试配置文件加载 ===~n~n"),
    
    %% 读取配置文件
    ConfigFile = "d:/github/eadm/config/db.config",
    io:format("读取配置文件: ~s~n", [ConfigFile]),
    
    case file:consult(ConfigFile) of
        {ok, Config} ->
            io:format("✓ 配置文件读取成功!~n~n"),
            
            %% 打印所有配置项
            io:format("配置项数量: ~p~n~n", [length(Config)]),
            
            lists:foreach(fun({AppName, AppConfig}) ->
                io:format("--- 应用: ~p ---~n", [AppName]),
                print_config(AppConfig, 1),
                io:format("~n")
            end, Config),
            
            %% 特别检查 emqx 和 tdengine 配置
            io:format("~n=== 验证 EMQX 配置结构 ===~n"),
            case lists:keyfind(emqx, 1, Config) of
                {emqx, EmqxConfig} ->
                    validate_pool_config(emqx, EmqxConfig);
                false ->
                    io:format("✗ 未找到 emqx 配置~n")
            end,
            
            io:format("~n=== 验证 TDengine 配置结构 ===~n"),
            case lists:keyfind(tdengine, 1, Config) of
                {tdengine, TdConfig} ->
                    validate_pool_config(tdengine, TdConfig);
                false ->
                    io:format("✗ 未找到 tdengine 配置~n")
            end,
            
            io:format("~n=== 测试完成 ===~n"),
            halt(0);
        {error, Reason} ->
            io:format("✗ 配置文件读取失败: ~p~n", [Reason]),
            halt(1)
    end;
    
main(_) ->
    io:format("用法: test_config.escript~n"),
    halt(1).

%% 打印配置内容（递归）
print_config([], _Indent) ->
    ok;
print_config([H|T], Indent) ->
    print_item(H, Indent),
    print_config(T, Indent);
print_config(Item, Indent) when not is_list(Item) ->
    print_item(Item, Indent).

print_item({Key, Value}, Indent) when is_list(Value) ->
    IndentStr = lists:duplicate(Indent * 2, $ ),
    io:format("~s~p:~n", [IndentStr, Key]),
    print_config(Value, Indent + 1);
print_item({Key, Value}, Indent) ->
    IndentStr = lists:duplicate(Indent * 2, $ ),
    io:format("~s~p: ~p~n", [IndentStr, Key, Value]);
print_item(Other, Indent) ->
    IndentStr = lists:duplicate(Indent * 2, $ ),
    io:format("~s~p~n", [IndentStr, Other]).

%% 验证连接池配置结构
validate_pool_config(AppName, Config) ->
    case proplists:get_value(pools, Config) of
        undefined ->
            io:format("✗ 缺少 pools 配置~n"),
            false;
        Pools ->
            io:format("✓ 找到 pools 配置~n"),
            case Pools of
                [{PoolName, PoolOpts, ConnOpts}] ->
                    io:format("  连接池名称: ~p~n", [PoolName]),
                    io:format("  池选项:~n"),
                    lists:foreach(fun({K, V}) ->
                        io:format("    ~p: ~p~n", [K, V])
                    end, PoolOpts),
                    io:format("  连接选项:~n"),
                    lists:foreach(fun({K, V}) ->
                        io:format("    ~p: ~p~n", [K, V])
                    end, ConnOpts),
                    io:format("✓ ~p 配置结构正确~n", [AppName]),
                    true;
                _ ->
                    io:format("✗ pools 结构不正确，应为 [{pool_name, [pool_opts], [conn_opts]}]~n"),
                    io:format("  实际结构: ~p~n", [Pools]),
                    false
            end
    end.
