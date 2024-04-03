%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2024, REDGREAT
%%% @doc
%%%
%%% 用户信息逻辑处理
%%%
%%% @end
%%% Created : 2024-03-20 16:20:17
%%%-------------------------------------------------------------------
-module(eadm_user_controller).
-author("wangcw").

%%%===================================================================
%%% Application callbacks
%%%===================================================================
-export([index/1, search/1, searchself/1, add/1, edit/1, editself/1, reset/1, password/1,
    delete/1, disable/1, userrole/1, userroleadd/1, userroledel/1, userpermission/1]).

%%====================================================================
%% API functions
%%====================================================================

%% @doc
%% index
%% @end
index(#{auth_data := #{<<"authed">> := true, <<"username">> := UserName}}) ->
    {ok, [{username, UserName}]};

index(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 查询返回数据结果
%% @end
search(#{auth_data := #{<<"authed">> := true, <<"permission">> := #{<<"usermanage">> := true}}}) ->
    try
        {ok, Res_Col, Res_Data} = mysql_pool:query(pool_db,
            "SELECT Id, TenantName, LoginName, UserName, Email, UserStatus, CreatedAt
            FROM vi_user
            ORDER BY CreatedAt;", []),
        Response = eadm_utils:return_as_json(Res_Col, Res_Data),
        {json, Response}
    catch
        _:Error ->
            Alert = #{<<"Alert">> => unicode:characters_to_binary("数据查询失败! " ++ binary_to_list(Error))},
            {json, [Alert]}
    end;

search(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 查询返回数据结果
%% @end
searchself(#{auth_data := #{<<"authed">> := true, <<"loginname">> := LoginName}}) ->
    try
        {ok, _, ResData} = mysql_pool:query(pool_db,
            "SELECT LoginName, UserName, Email
            FROM eadm_user
            WHERE LoginName=?
              AND UserStatus=0
              AND Deleted=0
            LIMIT 1", [LoginName]),
        {json, hd(ResData)}
    catch
        _:Error ->
            Alert = #{<<"Alert">> => unicode:characters_to_binary("数据查询失败! " ++ binary_to_list(Error))},
            {json, [Alert]}
    end;

searchself(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 新增用户数据
%% @end
add(#{auth_data := #{<<"authed">> := true, <<"loginname">> := CreatedUser},
      params := #{<<"loginName">> := LoginName, <<"email">> := Email,
      <<"userName">> := UserName, <<"password">> := PassWord}}) ->
    case validate_password(PassWord) of
        {ok} ->
            case validate_loginname(LoginName) of
                {ok} ->
                    case re:run(Email, "^[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\\.[a-zA-Z0-9-.]+$") of
                        {match, _} ->
                            try
                                CryptoGram = eadm_utils:pass_encrypt(PassWord),
                                mysql_pool:query(pool_db, "INSERT INTO eadm_user(Id, TenantId, LoginName, UserName, Email, CryptoGram, CreatedUser)
                                                          VALUES(fn_nextval('EU'), 'ET9999999997', ?, ?, ?, ?, ?);",
                                                          [LoginName, UserName, Email, CryptoGram, CreatedUser]),
                                A = unicode:characters_to_binary("用户【"),
                                B = unicode:characters_to_binary("】新增成功! "),
                                Info = #{<<"Alert">> => <<A/binary, UserName/binary, B/binary>>},
                                {json, [Info]}
                            catch
                                _:Error ->
                                    Alert = #{<<"Alert">> => unicode:characters_to_binary("用户新增失败！") ++ binary_to_list(Error)},
                                    {json, [Alert]}
                            end;
                        _ ->
                            A = unicode:characters_to_binary("邮箱【"),
                            B = unicode:characters_to_binary("】格式错误! "),
                            Info = #{<<"Alert">> => <<A/binary, Email/binary, B/binary>>},
                            {json, [Info]}
                    end;
                {error, 1} ->
                    A = unicode:characters_to_binary("登录名【"),
                    B = unicode:characters_to_binary("】不能少于6位! "),
                    Info = #{<<"Alert">> => <<A/binary, LoginName/binary, B/binary>>},
                    {json, [Info]};
                {error, 2} ->
                    A = unicode:characters_to_binary("登录名【"),
                    B = unicode:characters_to_binary("】不能大于18位! "),
                    Info = #{<<"Alert">> => <<A/binary, LoginName/binary, B/binary>>},
                    {json, [Info]};
                {error, 3} ->
                    A = unicode:characters_to_binary("登录名【"),
                    B = unicode:characters_to_binary("】已存在! "),
                    Info = #{<<"Alert">> => <<A/binary, LoginName/binary, B/binary>>},
                    {json, [Info]};
                {error, 6} ->
                    A = unicode:characters_to_binary("登录名【"),
                    B = unicode:characters_to_binary("】仅支持英文+数字! "),
                    Info = #{<<"Alert">> => <<A/binary, LoginName/binary, B/binary>>},
                    {json, [Info]};
                _ ->
                    Alert = #{<<"Alert">> => unicode:characters_to_binary("用户新增失败！")},
                    {json, [Alert]}
            end;
        {error, ErrInfo} ->
            Alert = #{<<"Alert">> => unicode:characters_to_binary(ErrInfo)},
            {json, [Alert]};
        _ ->
            Alert = #{<<"Alert">> => unicode:characters_to_binary("用户新增失败！")},
            {json, [Alert]}
    end;

add(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 编辑用户数据
%% @end
edit(#{auth_data := #{<<"authed">> := true, <<"loginname">> := CreatedUser},
      params := #{<<"userId">> := UserId, <<"loginName">> := LoginName,
          <<"email">> := Email, <<"userName">> := UserName}}) ->
      case validate_loginname(LoginName) of
          {ok} ->
              case re:run(Email, "^[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\\.[a-zA-Z0-9-.]+$") of
                  {match, _} ->
                      try
                          mysql_pool:query(pool_db,
                                "UPDATE eadm_user
                                SET LoginName=?,
                                UserName=?,
                                Email=?,
                                UpdatedUser=?
                                WHERE Id=?;",
                              [LoginName, UserName, Email, CreatedUser, UserId]),
                          A = unicode:characters_to_binary("用户【"),
                          B = unicode:characters_to_binary("】编辑成功! "),
                          Info = #{<<"Alert">> => <<A/binary, UserName/binary, B/binary>>},
                          {json, [Info]}
                      catch
                          _:Error ->
                              Alert = #{<<"Alert">> => unicode:characters_to_binary("用户编辑失败！") ++ binary_to_list(Error)},
                              {json, [Alert]}
                      end;
                  _ ->
                      A = unicode:characters_to_binary("邮箱【"),
                      B = unicode:characters_to_binary("】格式错误! "),
                      Info = #{<<"Alert">> => <<A/binary, Email/binary, B/binary>>},
                      {json, [Info]}
              end;
          {error, 1} ->
              A = unicode:characters_to_binary("登录名【"),
              B = unicode:characters_to_binary("】不能少于6位! "),
              Info = #{<<"Alert">> => <<A/binary, LoginName/binary, B/binary>>},
              {json, [Info]};
          {error, 2} ->
              A = unicode:characters_to_binary("登录名【"),
              B = unicode:characters_to_binary("】不能大于18位! "),
              Info = #{<<"Alert">> => <<A/binary, LoginName/binary, B/binary>>},
              {json, [Info]};
          {error, 3} ->
              A = unicode:characters_to_binary("登录名【"),
              B = unicode:characters_to_binary("】已存在! "),
              Info = #{<<"Alert">> => <<A/binary, LoginName/binary, B/binary>>},
              {json, [Info]};
          {error, 6} ->
              A = unicode:characters_to_binary("登录名【"),
              B = unicode:characters_to_binary("】仅支持英文+数字! "),
              Info = #{<<"Alert">> => <<A/binary, LoginName/binary, B/binary>>},
              {json, [Info]};
          _ ->
              Alert = #{<<"Alert">> => unicode:characters_to_binary("用户编辑失败！")},
              {json, [Alert]}
      end;

edit(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 编辑用户数据
%% @end
editself(#{auth_data := #{<<"authed">> := true, <<"loginname">> := CreatedUser},
      params := #{<<"loginName">> := LoginName, <<"email">> := Email, <<"userName">> := NewUserName}}) ->
    case re:run(Email, "^[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\\.[a-zA-Z0-9-.]+$") of
        {match, _} ->
            try
                mysql_pool:query(pool_db,
                      "UPDATE eadm_user
                      SET LoginName=?,
                      UserName=?,
                      Email=?,
                      UpdatedUser=?
                      WHERE LoginName=?;",
                    [LoginName, NewUserName, Email, CreatedUser, CreatedUser]),
                A = unicode:characters_to_binary("用户【"),
                B = unicode:characters_to_binary("】编辑成功! "),
                Info = #{<<"Alert">> => <<A/binary, NewUserName/binary, B/binary>>},
                {json, [Info]}
            catch
                _:Error ->
                    Alert = #{<<"Alert">> => unicode:characters_to_binary("用户编辑失败！") ++ binary_to_list(Error)},
                    {json, [Alert]}
            end;
        _ ->
            A = unicode:characters_to_binary("邮箱【"),
            B = unicode:characters_to_binary("】格式错误! "),
            Info = #{<<"Alert">> => <<A/binary, Email/binary, B/binary>>},
            {json, [Info]}
    end;
editself(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 修改用户密码
%% @end
password(#{auth_data := #{<<"authed">> := true, <<"loginname">> := LoginName},
      params := #{<<"passwordOld">> := PasswordOld, <<"passwordNew">> := PasswordNew}}) ->
    case validate_password(PasswordNew) of
        {ok} ->
            case eadm_utils:validate_login(LoginName, PasswordOld) of
                true ->
                    CryptoGram = eadm_utils:pass_encrypt(PasswordNew),
                    try
                        mysql_pool:query(pool_db,
                            "UPDATE eadm_user
                            SET UpdatedUser=?,
                            UpdatedAt=NOW(),
                            CryptoGram=?
                            WHERE LoginName=?
                              AND UserStatus=0
                              AND Deleted=0;",
                            [LoginName, CryptoGram, LoginName]),
                        Info = #{<<"Alert">> => unicode:characters_to_binary("密码修改成功! ")},
                        {json, [Info]}
                    catch
                        _:Error ->
                            Alert = #{<<"Alert">> => unicode:characters_to_binary("用户密码修改失败! ") ++ binary_to_list(Error)},
                            {json, [Alert]}
                    end;
                2 ->
                    lager:info("User Not Fond!"),
                    Alert = #{<<"Alert">> => unicode:characters_to_binary("用户不存在，请联系管理员！"),
                        <<"logined">> => 0},
                    {json, [Alert]};
                3 ->
                    lager:info("User Disable!"),
                    Alert = #{<<"Alert">> => unicode:characters_to_binary("用户已禁用，请联系管理员！"),
                        <<"logined">> => 0},
                    {json, [Alert]};
                _ ->
                    lager:info("User Login Failed!"),
                    Alert = #{<<"Alert">> => unicode:characters_to_binary("用户名或密码错误，请重新登录！"),
                        <<"logined">> => 0},
                    {json, [Alert]}
            end;
        {error, ErrInfo} ->
            Alert = #{<<"Alert">> => unicode:characters_to_binary(ErrInfo)},
            {json, [Alert]};
        _ ->
            Alert = #{<<"Alert">> => unicode:characters_to_binary("用户新增失败！")},
            {json, [Alert]}
    end;

password(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 重置用户密码
%% @end
reset(#{auth_data := #{<<"authed">> := true, <<"loginname">> := LoginName},
      bindings := #{<<"userId">> := UserId}}) ->
    % 重置密码123456
    CryptoGram = <<"4WpJ2hODluWuRFXsypv38CLIolSjGbe999q6gmCOa+0=">>,
    try
        mysql_pool:query(pool_db, "UPDATE eadm_user
                                  SET UpdatedUser = ?,
                                  UpdatedAt = NOW(),
                                  CryptoGram = ?
                                  WHERE Id = ?;",
                                  [LoginName, CryptoGram, UserId]),
        Info = #{<<"Alert">> => unicode:characters_to_binary("用户密码重置成功! ")},
        {json, [Info]}
    catch
        _:Error ->
            Alert = #{<<"Alert">> => unicode:characters_to_binary("用户密码重置失败! ") ++ binary_to_list(Error)},
            {json, [Alert]}
    end;

reset(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 禁用用户
%% @end
disable(#{auth_data := #{<<"authed">> := true, <<"loginname">> := LoginName},
     bindings := #{<<"userId">> := UserId}}) ->
    try
        mysql_pool:query(pool_db, "UPDATE eadm_user
                                  SET UserStatus = 1 - UserStatus,
                                      UpdatedUser = ?,
                                      UpdatedAt = CURRENT_TIMESTAMP()
                                  WHERE Id = ?
                                    AND Deleted = 0;",
                                  [LoginName, UserId]),
        Info = #{<<"Alert">> => unicode:characters_to_binary("用户启禁用成功! ")},
        {json, [Info]}
    catch
        _:Error ->
            Alert = #{<<"Alert">> => unicode:characters_to_binary("用户操作失败! ") ++ binary_to_list(Error)},
            {json, [Alert]}
    end;

disable(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 删除用户数据
%% @end
delete(#{auth_data := #{<<"authed">> := true, <<"loginname">> := LoginName},
    bindings := #{<<"userId">> := UserId}}) ->
    try
        mysql_pool:query(pool_db, "UPDATE eadm_user
                                  SET DeletedUser = ?,
                                  DeletedAt = NOW(),
                                  Deleted = 1
                                  WHERE Id = ?;",
                                  [LoginName, UserId]),
        Info = #{<<"Alert">> => unicode:characters_to_binary("用户删除成功! ")},
        {json, [Info]}
    catch
        _:Error ->
            Alert = #{<<"Alert">> => unicode:characters_to_binary("用户删除失败! ") ++ binary_to_list(Error)},
            {json, [Alert]}
    end;

delete(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 获取用户角色
%% @end
userrole(#{auth_data := #{<<"authed">> := true},
      bindings := #{<<"userId">> := UserId}}) ->
    try
        {ok, Res_Col, Res_Data} = mysql_pool:query(pool_db,
            "SELECT Id, RoleName, UpdatedAt
            FROM vi_userrole
            WHERE UserId=?;",
            [UserId]),
        Response = eadm_utils:return_as_json(Res_Col, Res_Data),
        {json, Response}
    catch
        _:Error ->
            Alert = #{<<"Alert">> => unicode:characters_to_binary("用户角色查询失败! ") ++ binary_to_list(Error)},
            {json, [Alert]}
    end;

userrole(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 新增用户角色
%% @end
userroleadd(#{auth_data := #{<<"authed">> := true, <<"loginname">> := LoginName}, params := RoleIdMap}) ->
    [{RoleIds, _Value}] = maps:to_list(RoleIdMap),
    {ok, RoleIdList} = thoas:decode(RoleIds),
    InsertQuery = "INSERT INTO eadm_userrole(UserId, RoleId, CreatedUser) VALUES(?, ?, ?);",
    try
        lists:foreach(fun (Map) ->
            mysql_pool:query(pool_db, InsertQuery,
                [maps:get(<<"userId">>, Map), maps:get(<<"roleId">>, Map), LoginName])
            end,
            RoleIdList),
        Info = #{<<"Alert">> => unicode:characters_to_binary("用户角色新增成功! ")},
        {json, [Info]}
    catch
        _:Error ->
            Alert = #{<<"Alert">> => unicode:characters_to_binary("用户角色新增失败! ") ++ binary_to_list(Error)},
            {json, [Alert]}
    end;

userroleadd(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 删除用户角色数据
%% @end
userroledel(#{auth_data := #{<<"authed">> := true, <<"loginname">> := LoginName},
    bindings := #{<<"userRoleId">> := UserRoleId}}) ->
    try
        mysql_pool:query(pool_db, "UPDATE eadm_userrole
                                  SET DeletedUser = ?,
                                  DeletedAt = NOW(),
                                  Deleted = 1
                                  WHERE Id = ?;",
                                  [LoginName, UserRoleId]),
        Info = #{<<"Alert">> => unicode:characters_to_binary("用户角色删除成功! ")},
        {json, [Info]}
    catch
        _:Error ->
            Alert = #{<<"Alert">> => unicode:characters_to_binary("用户角色删除失败! ") ++ binary_to_list(Error)},
            {json, [Alert]}
    end;

userroledel(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 获取角色权限数据
%% @end
userpermission(#{auth_data := #{<<"authed">> := true, <<"loginname">> := LoginName}}) ->
    Permission = get_permission(LoginName),
    {json, [Permission]};

userpermission(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%%====================================================================
%% Internal functions
%%====================================================================
%% @doc
%% 验证二进制密码数据
%% @end
validate_password(PassWordBin) when is_binary(PassWordBin) ->
    PassWord = binary_to_list(PassWordBin),
    AllowedChars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789,\._-",
    Regex = "^[" ++ AllowedChars ++ "]+$",
    case re:run(PassWord, Regex, [global, {capture, none}]) of
        match ->
            case byte_size(PassWordBin) of
                L when L < 6 ->
                    {error, "密码不能少于6位！"};
                L when L > 36 ->
                    {error, "密码不能大于36位！"};
                _ ->
                    {ok}
            end;
        _ ->
            {error, "密码仅支持【英文、数字、符号：,._-】"}
    end;
validate_password(_) ->
    {error, "密码格式错误！"}.

%% @doc
%% 验证登录名
%% @end
validate_loginname(LoginName) ->
    AllowedChars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_-",
    Regex = "^[" ++ AllowedChars ++ "]+$",
    case re:run(LoginName, Regex, [global, {capture, none}]) of
        match ->
            case byte_size(LoginName) of
                L when L < 6 ->
                    {error, 1};
                L when L > 18 ->
                    {error, 2};
                _ ->
                    try
                        case mysql_pool:query(pool_db, "SELECT 1 FROM eadm_user WHERE LoginName = ?;", [LoginName]) of
                            {ok, _, []} ->
                                {ok};
                            {ok, _, _} ->
                                {error, 3};
                            _ ->
                                {error, 4}
                        end
                    catch
                        _ ->
                            {error, 5}
                    end
            end;
        _ ->
            {error, 6}
    end.


%% @doc
%% 获取用户权限
%% @end
get_permission(LoginName) ->
    {ok, _, ResData} = mysql_pool:query(pool_db,
        "SELECT RolePermission
        FROM vi_userpermission
        WHERE LoginName=?
        LIMIT 1;", [LoginName]),
    {ok, ResMap} = thoas:decode(list_to_binary(ResData)),
    ResMap.

%% @doc
%% 验证邮箱格式
%% @end
validate_email(Email) ->
    case re:run(Email, "^[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\\.[a-zA-Z0-9-.]+$") of
        match ->
            true;
        _ ->
            false
    end.
