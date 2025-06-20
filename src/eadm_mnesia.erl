%%% @author wangcw
%%% @copyright (C) 2025, REDGREAT
%% @doc
%%
%% eadm mnesia数据库管理模块
%% 负责用户信息、角色信息及其关系的存储和管理
%%
%% @end
%%% Created : 2025-06-20
%%%-------------------------------------------------------------------
-module(eadm_mnesia).
-author("wangcw").

%%%===================================================================
%%% 函数导出
%%%===================================================================
-export([
    init/0,
    create_tables/0,
    
    %% 用户相关操作
    create_user/4,
    get_user/1,
    get_user_by_username/1,
    update_user/2,
    delete_user/1,
    list_users/0,
    
    %% 角色相关操作
    create_role/3,
    get_role/1,
    get_role_by_name/1,
    update_role/2,
    delete_role/1,
    list_roles/0,
    
    %% 用户角色关系操作
    assign_role_to_user/2,
    remove_role_from_user/2,
    get_user_roles/1,
    get_role_users/1
]).

%%%===================================================================
%%% 记录定义
%%%===================================================================
-record(eadm_user, {
    id,           %% 用户ID
    username,     %% 用户名
    password,     %% 密码
    email,        %% 邮箱
    created_at,   %% 创建时间
    updated_at    %% 更新时间
}).

-record(eadm_role, {
    id,           %% 角色ID
    name,         %% 角色名称
    description,  %% 角色描述
    created_at,   %% 创建时间
    updated_at    %% 更新时间
}).

-record(eadm_user_role, {
    user_id,      %% 用户ID
    role_id,      %% 角色ID
    assigned_at   %% 分配时间
}).

%%====================================================================
%% API 函数
%%====================================================================

%%--------------------------------------------------------------------
%% @doc
%% 初始化mnesia数据库
%% @end
%%--------------------------------------------------------------------
init() ->
    case mnesia:system_info(is_running) of
        yes ->
            lager:info("Mnesia已经在运行中"),
            create_tables();
        no ->
            lager:info("启动Mnesia数据库"),
            mnesia:start(),
            create_tables();
        starting ->
            lager:info("Mnesia正在启动中，等待启动完成"),
            timer:sleep(1000),
            init();
        stopping ->
            lager:info("Mnesia正在停止中，等待停止完成"),
            timer:sleep(1000),
            init()
    end.

%%--------------------------------------------------------------------
%% @doc
%% 创建数据表
%% @end
%%--------------------------------------------------------------------
create_tables() ->
    %% 创建用户表
    case mnesia:create_table(eadm_user, [
        {attributes, record_info(fields, eadm_user)},
        {disc_copies, [node()]},
        {type, set}
    ]) of
        {atomic, ok} ->
            lager:info("用户表创建成功");
        {aborted, {already_exists, eadm_user}} ->
            lager:info("用户表已存在");
        {aborted, Reason1} ->
            lager:error("用户表创建失败: ~p", [Reason1])
    end,
    
    %% 创建角色表
    case mnesia:create_table(eadm_role, [
        {attributes, record_info(fields, eadm_role)},
        {disc_copies, [node()]},
        {type, set}
    ]) of
        {atomic, ok} ->
            lager:info("角色表创建成功");
        {aborted, {already_exists, eadm_role}} ->
            lager:info("角色表已存在");
        {aborted, Reason2} ->
            lager:error("角色表创建失败: ~p", [Reason2])
    end,
    
    %% 创建用户角色关系表
    case mnesia:create_table(eadm_user_role, [
        {attributes, record_info(fields, eadm_user_role)},
        {disc_copies, [node()]},
        {type, bag}
    ]) of
        {atomic, ok} ->
            lager:info("用户角色关系表创建成功");
        {aborted, {already_exists, eadm_user_role}} ->
            lager:info("用户角色关系表已存在");
        {aborted, Reason3} ->
            lager:error("用户角色关系表创建失败: ~p", [Reason3])
    end,
    
    %% 创建索引
    mnesia:add_table_index(eadm_user, username),
    mnesia:add_table_index(eadm_role, name),
    ok.

%%====================================================================
%% 用户相关函数
%%====================================================================

%%--------------------------------------------------------------------
%% @doc
%% 创建用户
%% @end
%%--------------------------------------------------------------------
create_user(Username, Password, Email, Id) ->
    User = #eadm_user{
        id = Id,
        username = Username,
        password = Password,
        email = Email,
        created_at = erlang:system_time(second),
        updated_at = erlang:system_time(second)
    },
    Fun = fun() ->
        mnesia:write(User)
    end,
    case mnesia:transaction(Fun) of
        {atomic, ok} ->
            {ok, User};
        {aborted, Reason} ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc
%% 根据ID获取用户
%% @end
%%--------------------------------------------------------------------
get_user(Id) ->
    Fun = fun() ->
        mnesia:read(eadm_user, Id)
    end,
    case mnesia:transaction(Fun) of
        {atomic, [User]} ->
            {ok, User};
        {atomic, []} ->
            {error, not_found};
        {aborted, Reason} ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc
%% 根据用户名获取用户
%% @end
%%--------------------------------------------------------------------
get_user_by_username(Username) ->
    Fun = fun() ->
        mnesia:index_read(eadm_user, Username, username)
    end,
    case mnesia:transaction(Fun) of
        {atomic, [User]} ->
            {ok, User};
        {atomic, []} ->
            {error, not_found};
        {aborted, Reason} ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc
%% 更新用户
%% @end
%%--------------------------------------------------------------------
update_user(Id, UpdateFields) ->
    Fun = fun() ->
        case mnesia:read(eadm_user, Id) of
            [User] ->
                UpdatedUser = update_user_fields(User, UpdateFields),
                mnesia:write(UpdatedUser),
                UpdatedUser;
            [] ->
                {error, not_found}
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, {error, not_found}} ->
            {error, not_found};
        {atomic, User} ->
            {ok, User};
        {aborted, Reason} ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc
%% 删除用户
%% @end
%%--------------------------------------------------------------------
delete_user(Id) ->
    Fun = fun() ->
        %% 先删除用户角色关系
        UserRoles = mnesia:index_read(eadm_user_role, Id, user_id),
        lists:foreach(fun(UR) -> mnesia:delete_object(UR) end, UserRoles),
        %% 删除用户
        mnesia:delete({eadm_user, Id})
    end,
    case mnesia:transaction(Fun) of
        {atomic, ok} ->
            ok;
        {aborted, Reason} ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc
%% 获取所有用户列表
%% @end
%%--------------------------------------------------------------------
list_users() ->
    Fun = fun() ->
        mnesia:select(eadm_user, [{'_', [], ['$_']}])
    end,
    case mnesia:transaction(Fun) of
        {atomic, Users} ->
            {ok, Users};
        {aborted, Reason} ->
            {error, Reason}
    end.

%%====================================================================
%% 角色相关函数
%%====================================================================

%%--------------------------------------------------------------------
%% @doc
%% 创建角色
%% @end
%%--------------------------------------------------------------------
create_role(Name, Description, Id) ->
    Role = #eadm_role{
        id = Id,
        name = Name,
        description = Description,
        created_at = erlang:system_time(second),
        updated_at = erlang:system_time(second)
    },
    Fun = fun() ->
        mnesia:write(Role)
    end,
    case mnesia:transaction(Fun) of
        {atomic, ok} ->
            {ok, Role};
        {aborted, Reason} ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc
%% 根据ID获取角色
%% @end
%%--------------------------------------------------------------------
get_role(Id) ->
    Fun = fun() ->
        mnesia:read(eadm_role, Id)
    end,
    case mnesia:transaction(Fun) of
        {atomic, [Role]} ->
            {ok, Role};
        {atomic, []} ->
            {error, not_found};
        {aborted, Reason} ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc
%% 根据角色名获取角色
%% @end
%%--------------------------------------------------------------------
get_role_by_name(Name) ->
    Fun = fun() ->
        mnesia:index_read(eadm_role, Name, name)
    end,
    case mnesia:transaction(Fun) of
        {atomic, [Role]} ->
            {ok, Role};
        {atomic, []} ->
            {error, not_found};
        {aborted, Reason} ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc
%% 更新角色
%% @end
%%--------------------------------------------------------------------
update_role(Id, UpdateFields) ->
    Fun = fun() ->
        case mnesia:read(eadm_role, Id) of
            [Role] ->
                UpdatedRole = update_role_fields(Role, UpdateFields),
                mnesia:write(UpdatedRole),
                UpdatedRole;
            [] ->
                {error, not_found}
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, {error, not_found}} ->
            {error, not_found};
        {atomic, Role} ->
            {ok, Role};
        {aborted, Reason} ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc
%% 删除角色
%% @end
%%--------------------------------------------------------------------
delete_role(Id) ->
    Fun = fun() ->
        %% 先删除用户角色关系
        UserRoles = mnesia:index_read(eadm_user_role, Id, role_id),
        lists:foreach(fun(UR) -> mnesia:delete_object(UR) end, UserRoles),
        %% 删除角色
        mnesia:delete({eadm_role, Id})
    end,
    case mnesia:transaction(Fun) of
        {atomic, ok} ->
            ok;
        {aborted, Reason} ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc
%% 获取所有角色列表
%% @end
%%--------------------------------------------------------------------
list_roles() ->
    Fun = fun() ->
        mnesia:select(eadm_role, [{'_', [], ['$_']}])
    end,
    case mnesia:transaction(Fun) of
        {atomic, Roles} ->
            {ok, Roles};
        {aborted, Reason} ->
            {error, Reason}
    end.

%%====================================================================
%% 用户角色关系函数
%%====================================================================

%%--------------------------------------------------------------------
%% @doc
%% 为用户分配角色
%% @end
%%--------------------------------------------------------------------
assign_role_to_user(UserId, RoleId) ->
    UserRole = #eadm_user_role{
        user_id = UserId,
        role_id = RoleId,
        assigned_at = erlang:system_time(second)
    },
    Fun = fun() ->
        %% 检查关系是否已存在
        case mnesia:match_object(#eadm_user_role{user_id = UserId, role_id = RoleId, _ = '_'}) of
            [] ->
                mnesia:write(UserRole);
            _ ->
                {error, already_exists}
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, ok} ->
            ok;
        {atomic, {error, already_exists}} ->
            {error, already_exists};
        {aborted, Reason} ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc
%% 移除用户的角色
%% @end
%%--------------------------------------------------------------------
remove_role_from_user(UserId, RoleId) ->
    Fun = fun() ->
        case mnesia:match_object(#eadm_user_role{user_id = UserId, role_id = RoleId, _ = '_'}) of
            [UserRole] ->
                mnesia:delete_object(UserRole);
            [] ->
                {error, not_found}
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, ok} ->
            ok;
        {atomic, {error, not_found}} ->
            {error, not_found};
        {aborted, Reason} ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc
%% 获取用户的所有角色
%% @end
%%--------------------------------------------------------------------
get_user_roles(UserId) ->
    Fun = fun() ->
        UserRoles = mnesia:index_read(eadm_user_role, UserId, user_id),
        RoleIds = [UR#eadm_user_role.role_id || UR <- UserRoles],
        lists:foldl(fun(RoleId, Acc) ->
            case mnesia:read(eadm_role, RoleId) of
                [Role] -> [Role | Acc];
                [] -> Acc
            end
        end, [], RoleIds)
    end,
    case mnesia:transaction(Fun) of
        {atomic, Roles} ->
            {ok, Roles};
        {aborted, Reason} ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc
%% 获取角色下的所有用户
%% @end
%%--------------------------------------------------------------------
get_role_users(RoleId) ->
    Fun = fun() ->
        UserRoles = mnesia:index_read(eadm_user_role, RoleId, role_id),
        UserIds = [UR#eadm_user_role.user_id || UR <- UserRoles],
        lists:foldl(fun(UserId, Acc) ->
            case mnesia:read(eadm_user, UserId) of
                [User] -> [User | Acc];
                [] -> Acc
            end
        end, [], UserIds)
    end,
    case mnesia:transaction(Fun) of
        {atomic, Users} ->
            {ok, Users};
        {aborted, Reason} ->
            {error, Reason}
    end.

%%====================================================================
%% 内部函数
%%====================================================================

%%--------------------------------------------------------------------
%% @doc
%% 更新用户字段
%% @end
%%--------------------------------------------------------------------
update_user_fields(User, []) ->
    User#eadm_user{updated_at = erlang:system_time(second)};
update_user_fields(User, [{username, Username} | Rest]) ->
    update_user_fields(User#eadm_user{username = Username}, Rest);
update_user_fields(User, [{password, Password} | Rest]) ->
    update_user_fields(User#eadm_user{password = Password}, Rest);
update_user_fields(User, [{email, Email} | Rest]) ->
    update_user_fields(User#eadm_user{email = Email}, Rest);
update_user_fields(User, [_ | Rest]) ->
    update_user_fields(User, Rest).

%%--------------------------------------------------------------------
%% @doc
%% 更新角色字段
%% @end
%%--------------------------------------------------------------------
update_role_fields(Role, []) ->
    Role#eadm_role{updated_at = erlang:system_time(second)};
update_role_fields(Role, [{name, Name} | Rest]) ->
    update_role_fields(Role#eadm_role{name = Name}, Rest);
update_role_fields(Role, [{description, Description} | Rest]) ->
    update_role_fields(Role#eadm_role{description = Description}, Rest);
update_role_fields(Role, [_ | Rest]) ->
    update_role_fields(Role, Rest).