-module(migrate_sports).
-export([run/0]).

run() ->
    % 启动应用
    application:ensure_all_started(eadm),

    % 模拟用户权限，运行迁移
    AuthData = #{
        <<"authed">> => true,
        <<"permission">> => #{
            <<"usermanage">> => true
        },
        <<"loginname">> => <<"wangcw">>
    },

    Params = #{auth_data => AuthData},

    % 调用迁移函数
    Result = eadm_sys_migrate_controller:sports_permission(Params),

    io:format("迁移结果: ~p~n", [Result]),

    % 停止应用
    application:stop(eadm).
