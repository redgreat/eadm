%%% @author wangcw
%%% @copyright (C) 2024, REDGREAT
%%% @doc
%%%
%%% 基于mnesia数据库的用户信息逻辑处理
%%%
%%% @end
%%% Created : 2024-12-19
%%%-------------------------------------------------------------------
-module(eadm_user_controller).
-author("wangcw").

%%%===================================================================
%%% 函数导出
%%%===================================================================
-export([index/1, search/1, add/1, edit/1, reset/1, delete/1, disable/1,
    userrole/1, userroleadd/1, userroledel/1, userpermission/1]).

%%====================================================================
%% API 函数
%%====================================================================

%% @doc
%% 主函数
%% @end
index(#{auth_data := #{<<"authed">> := true, <<"username">> := UserName,
      <<"permission">> := #{<<"usermanage">> := true}}}) ->
    {ok, [{username, UserName}]};

index(#{auth_data := #{<<"permission">> := #{<<"usermanage">> := false}}}) ->
    {json, [#{<<"Alert">> => unicode:characters_to_binary("API鉴权失败！", utf8)}]};

index(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 查询返回数据结果
%% @end
search(#{auth_data := #{<<"authed">> := true, <<"permission">> := #{<<"usermanage">> := true}}}) ->
    try
        case eadm_mnesia:list_users() of
            {ok, Users} ->
                Response = format_users_response(Users),
                {json, Response};
            {error, Reason} ->
                lager:error("用户查询失败：~p~n", [Reason]),
                {json, [#{<<"Alert">> => unicode:characters_to_binary("用户查询失败！", utf8)}]}
        end
    catch
        _:Error ->
            lager:error("用户查询失败：~p~n", [Error]),
            {json, [#{<<"Alert">> => unicode:characters_to_binary("用户查询失败！", utf8)}]}
    end;

search(#{auth_data := #{<<"permission">> := #{<<"usermanage">> := false}}}) ->
    {json, [#{<<"Alert">> => unicode:characters_to_binary("API鉴权失败！")}]};

search(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 新增用户数据
%% @end
add(#{auth_data := #{<<"authed">> := true, <<"loginname">> := CreatedUser,
      <<"permission">> := #{<<"usermanage">> := true}},
      params := #{<<"loginName">> := LoginName, <<"email">> := Email,
      <<"userName">> := UserName, <<"password">> := PassWord}}) ->
    try
        case validate_password(PassWord) of
            {ok} ->
                case validate_addloginname(LoginName) of
                    {ok} ->
                        case re:run(Email, "^[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\\.[a-zA-Z0-9-.]+$") of
                            {match, _} ->
                                CryptoGram = eadm_utils:pass_encrypt(PassWord),
                                UserId = generate_user_id(),
                                case eadm_mnesia:create_user(LoginName, CryptoGram, Email, UserId) of
                                    {ok, _User} ->
                                        A = unicode:characters_to_binary("用户【", utf8),
                                        B = unicode:characters_to_binary("】新增成功！", utf8),
                                        {json, [#{<<"Alert">> => <<A/binary, UserName/binary, B/binary>>}]};
                                    {error, Reason} ->
                                        lager:error("用户新增失败：~p~n", [Reason]),
                                        {json, [#{<<"Alert">> => unicode:characters_to_binary("用户新增失败！", utf8)}]}
                                end;
                            _ ->
                                A = unicode:characters_to_binary("邮箱【", utf8),
                                B = unicode:characters_to_binary("】格式错误！", utf8),
                                {json, [#{<<"Alert">> => <<A/binary, Email/binary, B/binary>>}]}
                        end;
                    {error, 1} ->
                        A = unicode:characters_to_binary("登录名【", utf8),
                        B = unicode:characters_to_binary("】不能少于6位！", utf8),
                        {json, [#{<<"Alert">> => <<A/binary, LoginName/binary, B/binary>>}]};
                    {error, 2} ->
                        A = unicode:characters_to_binary("登录名【", utf8),
                        B = unicode:characters_to_binary("】不能大于18位！", utf8),
                        {json, [#{<<"Alert">> => <<A/binary, LoginName/binary, B/binary>>}]};
                    {error, 3} ->
                        A = unicode:characters_to_binary("登录名【", utf8),
                        B = unicode:characters_to_binary("】已存在！", utf8),
                        {json, [#{<<"Alert">> => <<A/binary, LoginName/binary, B/binary>>}]};
                    {error, 6} ->
                        A = unicode:characters_to_binary("登录名【", utf8),
                        B = unicode:characters_to_binary("】仅支持英文+数字！", utf8),
                        {json, [#{<<"Alert">> => <<A/binary, LoginName/binary, B/binary>>}]};
                    _ ->
                        {json, [#{<<"Alert">> => unicode:characters_to_binary("用户新增失败！", utf8)}]}
                end;
            {error, ErrInfo} ->
                {json, [#{<<"Alert">> => unicode:characters_to_binary(ErrInfo, utf8)}]};
            _ ->
                {json, [#{<<"Alert">> => unicode:characters_to_binary("用户新增失败！", utf8)}]}
        end
    catch
        _:Error ->
            lager:error("用户新增失败：~p~n", [Error]),
            {json, [#{<<"Alert">> => unicode:characters_to_binary("用户新增失败！", utf8)}]}
    end;

add(#{auth_data := #{<<"permission">> := #{<<"usermanage">> := false}}}) ->
    {json, [#{<<"Alert">> => unicode:characters_to_binary("API鉴权失败！", utf8)}]};

add(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 编辑用户数据
%% @end
edit(#{auth_data := #{<<"authed">> := true, <<"loginname">> := CreatedUser,
      <<"permission">> := #{<<"usermanage">> := true}},
      params := #{<<"userId">> := UserId, <<"loginName">> := LoginName,
          <<"email">> := Email, <<"userName">> := UserName}}) ->
      case validate_editloginname(UserId, LoginName) of
          {ok} ->
              case re:run(Email, "^[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\\.[a-zA-Z0-9-.]+$") of
                  {match, _} ->
                      try
                          UpdateFields = [{username, LoginName}, {email, Email}],
                          case eadm_mnesia:update_user(UserId, UpdateFields) of
                              {ok, _User} ->
                                  A = unicode:characters_to_binary("用户【", utf8),
                                  B = unicode:characters_to_binary("】编辑成功！", utf8),
                                  {json, [#{<<"Alert">> => <<A/binary, UserName/binary, B/binary>>}]};
                              {error, Reason} ->
                                  lager:error("用户编辑失败：~p~n", [Reason]),
                                  {json, [#{<<"Alert">> => unicode:characters_to_binary("用户编辑失败！", utf8)}]}
                          end
                      catch
                          _:Error ->
                              lager:error("用户编辑失败：~p~n", [Error]),
                              {json, [#{<<"Alert">> => unicode:characters_to_binary("用户编辑失败！", utf8)}]}
                      end;
                  _ ->
                      A = unicode:characters_to_binary("邮箱【", utf8),
                      B = unicode:characters_to_binary("】格式错误！", utf8),
                      {json, [#{<<"Alert">> => <<A/binary, Email/binary, B/binary>>}]}
              end;
          {error, 1} ->
              A = unicode:characters_to_binary("登录名【", utf8),
              B = unicode:characters_to_binary("】不能少于6位！", utf8),
              {json, [#{<<"Alert">> => <<A/binary, LoginName/binary, B/binary>>}]};
          {error, 2} ->
              A = unicode:characters_to_binary("登录名【", utf8),
              B = unicode:characters_to_binary("】不能大于18位！", utf8),
              {json, [#{<<"Alert">> => <<A/binary, LoginName/binary, B/binary>>}]};
          {error, 3} ->
              A = unicode:characters_to_binary("登录名【", utf8),
              B = unicode:characters_to_binary("】已存在！", utf8),
              {json, [#{<<"Alert">> => <<A/binary, LoginName/binary, B/binary>>}]};
          {error, 6} ->
              A = unicode:characters_to_binary("登录名【", utf8),
              B = unicode:characters_to_binary("】仅支持英文+数字！", utf8),
              {json, [#{<<"Alert">> => <<A/binary, LoginName/binary, B/binary>>}]};
          _ ->
              {json, [#{<<"Alert">> => unicode:characters_to_binary("用户编辑失败！", utf8)}]}
      end;

edit(#{auth_data := #{<<"permission">> := #{<<"usermanage">> := false}}}) ->
    {json, [#{<<"Alert">> => unicode:characters_to_binary("API鉴权失败！", utf8)}]};

edit(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 重置用户密码
%% @end
reset(#{auth_data := #{<<"authed">> := true, <<"loginname">> := LoginName,
      <<"permission">> := #{<<"usermanage">> := true}},
      bindings := #{<<"userId">> := UserId}}) ->
    CryptoGram = eadm_utils:pass_encrypt(<<"123456">>),
    lager:info("用户~p重置了密码~n", [LoginName]),
    try
        case eadm_mnesia:update_user(UserId, [{password, CryptoGram}]) of
            {ok, _User} ->
                {json, [#{<<"Alert">> => unicode:characters_to_binary("用户密码重置成功！", utf8)}]};
            {error, Reason} ->
                lager:error("用户密码重置失败：~p~n", [Reason]),
                {json, [#{<<"Alert">> => unicode:characters_to_binary("用户密码重置失败！", utf8)}]}
        end
    catch
        _:Error ->
            lager:error("用户密码重置失败：~p~n", [Error]),
            {json, [#{<<"Alert">> => unicode:characters_to_binary("用户密码重置失败！", utf8)}]}
    end;

reset(#{auth_data := #{<<"permission">> := #{<<"usermanage">> := false}}}) ->
    {json, [#{<<"Alert">> => unicode:characters_to_binary("API鉴权失败！", utf8)}]};

reset(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 删除用户
%% @end
delete(#{auth_data := #{<<"authed">> := true, <<"loginname">> := LoginName,
       <<"permission">> := #{<<"usermanage">> := true}},
       bindings := #{<<"userId">> := UserId}}) ->
    lager:info("用户~p删除了用户~p~n", [LoginName, UserId]),
    try
        case eadm_mnesia:delete_user(UserId) of
            ok ->
                {json, [#{<<"Alert">> => unicode:characters_to_binary("用户删除成功！", utf8)}]};
            {error, Reason} ->
                lager:error("用户删除失败：~p~n", [Reason]),
                {json, [#{<<"Alert">> => unicode:characters_to_binary("用户删除失败！", utf8)}]}
        end
    catch
        _:Error ->
            lager:error("用户删除失败：~p~n", [Error]),
            {json, [#{<<"Alert">> => unicode:characters_to_binary("用户删除失败！", utf8)}]}
    end;

delete(#{auth_data := #{<<"permission">> := #{<<"usermanage">> := false}}}) ->
    {json, [#{<<"Alert">> => unicode:characters_to_binary("API鉴权失败！", utf8)}]};

delete(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 禁用用户
%% @end
disable(#{auth_data := #{<<"authed">> := true, <<"loginname">> := LoginName,
        <<"permission">> := #{<<"usermanage">> := true}},
        bindings := #{<<"userId">> := UserId}}) ->
    lager:info("用户~p禁用了用户~p~n", [LoginName, UserId]),
    try
        case eadm_mnesia:update_user(UserId, [{status, disabled}]) of
            {ok, _User} ->
                {json, [#{<<"Alert">> => unicode:characters_to_binary("用户禁用成功！", utf8)}]};
            {error, Reason} ->
                lager:error("用户禁用失败：~p~n", [Reason]),
                {json, [#{<<"Alert">> => unicode:characters_to_binary("用户禁用失败！", utf8)}]}
        end
    catch
        _:Error ->
            lager:error("用户禁用失败：~p~n", [Error]),
            {json, [#{<<"Alert">> => unicode:characters_to_binary("用户禁用失败！", utf8)}]}
    end;

disable(#{auth_data := #{<<"permission">> := #{<<"usermanage">> := false}}}) ->
    {json, [#{<<"Alert">> => unicode:characters_to_binary("API鉴权失败！", utf8)}]};

disable(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 用户角色管理页面
%% @end
userrole(#{auth_data := #{<<"authed">> := true, <<"username">> := UserName,
         <<"permission">> := #{<<"usermanage">> := true}},
         bindings := #{<<"userId">> := UserId}}) ->
    case eadm_mnesia:get_user(UserId) of
        {ok, User} ->
            {ok, [{username, UserName}, {user_info, User}]};
        {error, _Reason} ->
            {json, [#{<<"Alert">> => unicode:characters_to_binary("用户不存在！", utf8)}]}
    end;

userrole(#{auth_data := #{<<"permission">> := #{<<"usermanage">> := false}}}) ->
    {json, [#{<<"Alert">> => unicode:characters_to_binary("API鉴权失败！", utf8)}]};

userrole(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 为用户添加角色
%% @end
userroleadd(#{auth_data := #{<<"authed">> := true,
             <<"permission">> := #{<<"usermanage">> := true}},
             params := #{<<"userId">> := UserId, <<"roleId">> := RoleId}}) ->
    try
        case eadm_mnesia:assign_role_to_user(UserId, RoleId) of
            ok ->
                {json, [#{<<"Alert">> => unicode:characters_to_binary("角色分配成功！", utf8)}]};
            {error, already_exists} ->
                {json, [#{<<"Alert">> => unicode:characters_to_binary("用户已拥有该角色！", utf8)}]};
            {error, Reason} ->
                lager:error("角色分配失败：~p~n", [Reason]),
                {json, [#{<<"Alert">> => unicode:characters_to_binary("角色分配失败！", utf8)}]}
        end
    catch
        _:Error ->
            lager:error("角色分配失败：~p~n", [Error]),
            {json, [#{<<"Alert">> => unicode:characters_to_binary("角色分配失败！", utf8)}]}
    end;

userroleadd(#{auth_data := #{<<"permission">> := #{<<"usermanage">> := false}}}) ->
    {json, [#{<<"Alert">> => unicode:characters_to_binary("API鉴权失败！", utf8)}]};

userroleadd(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 移除用户角色
%% @end
userroledel(#{auth_data := #{<<"authed">> := true,
             <<"permission">> := #{<<"usermanage">> := true}},
             params := #{<<"userId">> := UserId, <<"roleId">> := RoleId}}) ->
    try
        case eadm_mnesia:remove_role_from_user(UserId, RoleId) of
            ok ->
                {json, [#{<<"Alert">> => unicode:characters_to_binary("角色移除成功！", utf8)}]};
            {error, not_found} ->
                {json, [#{<<"Alert">> => unicode:characters_to_binary("用户未拥有该角色！", utf8)}]};
            {error, Reason} ->
                lager:error("角色移除失败：~p~n", [Reason]),
                {json, [#{<<"Alert">> => unicode:characters_to_binary("角色移除失败！", utf8)}]}
        end
    catch
        _:Error ->
            lager:error("角色移除失败：~p~n", [Error]),
            {json, [#{<<"Alert">> => unicode:characters_to_binary("角色移除失败！", utf8)}]}
    end;

userroledel(#{auth_data := #{<<"permission">> := #{<<"usermanage">> := false}}}) ->
    {json, [#{<<"Alert">> => unicode:characters_to_binary("API鉴权失败！", utf8)}]};

userroledel(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 获取用户权限信息
%% @end
userpermission(#{auth_data := #{<<"authed">> := true,
               <<"permission">> := #{<<"usermanage">> := true}},
               bindings := #{<<"userId">> := UserId}}) ->
    try
        case eadm_mnesia:get_user_roles(UserId) of
            {ok, Roles} ->
                Response = format_roles_response(Roles),
                {json, Response};
            {error, Reason} ->
                lager:error("用户权限查询失败：~p~n", [Reason]),
                {json, [#{<<"Alert">> => unicode:characters_to_binary("用户权限查询失败！", utf8)}]}
        end
    catch
        _:Error ->
            lager:error("用户权限查询失败：~p~n", [Error]),
            {json, [#{<<"Alert">> => unicode:characters_to_binary("用户权限查询失败！", utf8)}]}
    end;

userpermission(#{auth_data := #{<<"permission">> := #{<<"usermanage">> := false}}}) ->
    {json, [#{<<"Alert">> => unicode:characters_to_binary("API鉴权失败！", utf8)}]};

userpermission(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%%====================================================================
%% 内部函数
%%====================================================================

%% @doc
%% 格式化用户响应数据
%% @end
format_users_response(Users) ->
    lists:map(fun(User) ->
        #{<<"id">> => element(2, User),
          <<"username">> => element(3, User),
          <<"email">> => element(5, User),
          <<"created_at">> => element(6, User),
          <<"updated_at">> => element(7, User)}
    end, Users).

%% @doc
%% 格式化角色响应数据
%% @end
format_roles_response(Roles) ->
    lists:map(fun(Role) ->
        #{<<"id">> => element(2, Role),
          <<"name">> => element(3, Role),
          <<"description">> => element(4, Role),
          <<"created_at">> => element(5, Role),
          <<"updated_at">> => element(6, Role)}
    end, Roles).

%% @doc
%% 生成用户ID
%% @end
generate_user_id() ->
    erlang:system_time(microsecond).

%% @doc
%% 验证密码
%% @end
validate_password(Password) ->
    case byte_size(Password) of
        Size when Size < 6 ->
            {error, "密码不能少于6位！"};
        Size when Size > 20 ->
            {error, "密码不能大于20位！"};
        _ ->
            {ok}
    end.

%% @doc
%% 验证新增登录名
%% @end
validate_addloginname(LoginName) ->
    case byte_size(LoginName) of
        Size when Size < 6 ->
            {error, 1};
        Size when Size > 18 ->
            {error, 2};
        _ ->
            case re:run(LoginName, "^[a-zA-Z0-9]+$") of
                {match, _} ->
                    case eadm_mnesia:get_user_by_username(LoginName) of
                        {ok, _User} ->
                            {error, 3};
                        {error, not_found} ->
                            {ok};
                        {error, _} ->
                            {error, 4}
                    end;
                _ ->
                    {error, 6}
            end
    end.

%% @doc
%% 验证编辑登录名
%% @end
validate_editloginname(UserId, LoginName) ->
    case byte_size(LoginName) of
        Size when Size < 6 ->
            {error, 1};
        Size when Size > 18 ->
            {error, 2};
        _ ->
            case re:run(LoginName, "^[a-zA-Z0-9]+$") of
                {match, _} ->
                    case eadm_mnesia:get_user_by_username(LoginName) of
                        {ok, User} ->
                            case element(2, User) of
                                UserId ->
                                    {ok};
                                _ ->
                                    {error, 3}
                            end;
                        {error, not_found} ->
                            {ok};
                        {error, _} ->
                            {error, 4}
                    end;
                _ ->
                    {error, 6}
            end
    end.