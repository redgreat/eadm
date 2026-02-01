#!/usr/bin/env escript
%% 热加载修改的模块

main(_) ->
    % 连接到运行中的节点
    Node = 'eadm@127.0.0.1',
    case net_adm:ping(Node) of
        pong ->
            io:format("成功连接到节点: ~p~n", [Node]),

            % 重新加载模块
            Modules = [eadm_auth, eadm_user_controller, eadm_utils],
            lists:foreach(
                fun(Mod) ->
                    case rpc:call(Node, code, purge, [Mod]) of
                        true -> io:format("清除模块: ~p~n", [Mod]);
                        false -> io:format("模块未加载: ~p~n", [Mod])
                    end,
                    case rpc:call(Node, code, load_file, [Mod]) of
                        {module, Mod} -> io:format("重新加载模块成功: ~p~n", [Mod]);
                        {error, Reason} -> io:format("重新加载模块失败: ~p, 原因: ~p~n", [Mod, Reason])
                    end
                end,
                Modules
            ),

            io:format("~n模块重新加载完成！~n");
        pang ->
            io:format("无法连接到节点: ~p~n请确保应用正在运行~n", [Node]),
            halt(1)
    end.
