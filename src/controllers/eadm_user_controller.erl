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
        {ok, Res_Col, Res_Data} = eadm_pgpool:equery(pool_pg,
            "select id, tenantname, loginname, username, email, userstatus, createdat
            from vi_user order by createdat;", []),
        Response = eadm_utils:pg_as_json(Res_Col, Res_Data),
        {json, Response}
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
                                eadm_pgpool:equery(pool_pg, "insert into eadm_user(tenantid, loginname, username, email, passwd, createduser)
                                                          values('et0000000002', $1, $2, $3, $4, $5);",
                                                          [LoginName, UserName, Email, CryptoGram, CreatedUser]),
                                A = unicode:characters_to_binary("用户【", utf8),
                                B = unicode:characters_to_binary("】新增成功！", utf8),
                                {json, [#{<<"Alert">> => <<A/binary, UserName/binary, B/binary>>}]};
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
                          eadm_pgpool:equery(pool_pg,
                                "update eadm_user
                                set loginname = $1,
                                username = $2,
                                email = $3,
                                updateduser = $4
                                where id = $5;",
                              [LoginName, UserName, Email, CreatedUser, UserId]),
                          A = unicode:characters_to_binary("用户【", utf8),
                          B = unicode:characters_to_binary("】编辑成功！", utf8),
                          {json, [#{<<"Alert">> => <<A/binary, UserName/binary, B/binary>>}]}
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
    % 重置密码123456
    CryptoGram = <<"4WpJ2hODluWuRFXsypv38CLIolSjGbe999q6gmCOa+0=">>,
    try
        eadm_pgpool:equery(pool_pg, "update eadm_user
                         set updateduser = $1,
                         updatedat = current_timestamp,
                         passwd = $2
                         where id = $3;",
                         [LoginName, CryptoGram, UserId]),
        {json, [#{<<"Alert">> => unicode:characters_to_binary("用户密码重置成功！", utf8)}]}
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
%% 禁用用户
%% @end
disable(#{auth_data := #{<<"authed">> := true, <<"loginname">> := LoginName,
      <<"permission">> := #{<<"usermanage">> := true}},
     bindings := #{<<"userId">> := UserId}}) ->
    try
        eadm_pgpool:equery(pool_pg, "update eadm_user
                                  set userstatus= 1 - userstatus,
                                      updateduser = $1,
                                      updatedat = current_timestamp
                                  where id = $2
                                    and deleted is false;",
                                  [LoginName, UserId]),
        {json, [#{<<"Alert">> => unicode:characters_to_binary("用户启禁用成功！", utf8)}]}
    catch
        _:Error ->
            lager:error("用户操作失败：~p~n", [Error]),
            {json, [#{<<"Alert">> => unicode:characters_to_binary("用户操作失败！", utf8)}]}
    end;

disable(#{auth_data := #{<<"permission">> := #{<<"usermanage">> := false}}}) ->
    {json, [#{<<"Alert">> => unicode:characters_to_binary("API鉴权失败！", utf8)}]};

disable(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 删除用户数据
%% @end
delete(#{auth_data := #{<<"authed">> := true, <<"loginname">> := LoginName,
      <<"permission">> := #{<<"usermanage">> := true}},
    bindings := #{<<"userId">> := UserId}}) ->
    try
        eadm_pgpool:equery(pool_pg, "update eadm_user
                                  set deleteduser = $1,
                                  deletedat = current_timestamp,
                                  deleted = true
                                  where id = $2;",
                                  [LoginName, UserId]),
        {json, [#{<<"Alert">> => unicode:characters_to_binary("用户删除成功！", utf8)}]}
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
%% 获取用户角色
%% @end
userrole(#{auth_data := #{<<"authed">> := true,
      <<"permission">> := #{<<"usermanage">> := true}},
      bindings := #{<<"userId">> := UserId}}) ->
    try
        {ok, ResCol, ResData} = eadm_pgpool:equery(pool_pg,
            "select id, rolename, updatedat
            from vi_userrole
            where userid = $1;",
            [UserId]),
        {json, eadm_utils:pg_as_json(ResCol, ResData)}
    catch
        _:Error ->
            lager:error("用户角色查询失败：~p~n", [Error]),
            {json, [#{<<"Alert">> => unicode:characters_to_binary("用户角色查询失败！", utf8)}]}
    end;

userrole(#{auth_data := #{<<"permission">> := #{<<"usermanage">> := false}}}) ->
    {json, [#{<<"Alert">> => unicode:characters_to_binary("API鉴权失败！", utf8)}]};

userrole(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 新增用户角色
%% @end
userroleadd(#{auth_data := #{<<"authed">> := true, <<"loginname">> := LoginName,
      <<"permission">> := #{<<"usermanage">> := true}}, params := RoleIdMap}) ->
    [{RoleIds, _Value}] = maps:to_list(RoleIdMap),
    {ok, RoleIdList} = thoas:decode(RoleIds),
    InsertQuery = "insert into eadm_userrole(userid, roleid, createduser) values($1, $2, $3);",
    try
        lists:foreach(fun (Map) ->
            eadm_pgpool:equery(pool_pg, InsertQuery,
                [maps:get(<<"userId">>, Map), maps:get(<<"roleId">>, Map), LoginName])
            end,
            RoleIdList),
        {json, [#{<<"Alert">> => unicode:characters_to_binary("用户角色新增成功！", utf8)}]}
    catch
        _:Error ->
            lager:error("用户角色新增失败：~p~n", [Error]),
            {json, [#{<<"Alert">> => unicode:characters_to_binary("用户角色新增失败！", utf8)}]}
    end;

userroleadd(#{auth_data := #{<<"permission">> := #{<<"usermanage">> := false}}}) ->
    {json, [#{<<"Alert">> => unicode:characters_to_binary("API鉴权失败！", utf8)}]};

userroleadd(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 删除用户角色数据
%% @end
userroledel(#{auth_data := #{<<"authed">> := true, <<"loginname">> := LoginName,
      <<"permission">> := #{<<"usermanage">> := true}},
      bindings := #{<<"userRoleId">> := UserRoleId}}) ->
    try
        eadm_pgpool:equery(pool_pg, "update eadm_userrole
                                  set deleteduser = $1,
                                  deletedat = current_timestamp,
                                  deleted = true
                                  where id = $2;",
                                  [LoginName, erlang:binary_to_integer(UserRoleId)]),
        {json, [#{<<"Alert">> => unicode:characters_to_binary("用户角色删除成功！", utf8)}]}
    catch
        _:Error ->
            lager:error("用户角色删除失败：~p~n", [Error]),
            {json, [#{<<"Alert">> => unicode:characters_to_binary("用户角色删除失败！", utf8)}]}
    end;

userroledel(#{auth_data := #{<<"permission">> := #{<<"usermanage">> := false}}}) ->
    {json, [#{<<"Alert">> => unicode:characters_to_binary("API鉴权失败！", utf8)}]};

userroledel(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 获取角色权限数据
%% 需特殊处理权限验证，需要登录成功所以要验authed=true，无需数据权限不需验permission
%% @end
userpermission(#{auth_data := #{<<"authed">> := true, <<"loginname">> := LoginName}}) ->
    Permission = get_permission(LoginName),
    {json, [Permission]};

userpermission(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%%====================================================================
%% 内部函数
%%====================================================================

%% @doc
%% 验证登录名是否有重复(新增)
%% @end
validate_addloginname(LoginName) ->
    AllowedChars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_-",
    Regex = "^[" ++ AllowedChars ++ "]+$",
    try
        case re:run(LoginName, Regex, [global, {capture, none}]) of
            match ->
                case erlang:byte_size(LoginName) of
                    L when L < 6 ->
                        {error, 1};
                    L when L > 18 ->
                        {error, 2};
                    _ ->
                        try
                            case eadm_pgpool:equery(pool_pg, "select 1 from eadm_user where loginname = $1;", [LoginName]) of
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
        end
    catch
        _:Error ->
            lager:error("用户名验证失败：~p~n", [Error]),
            {json, [#{<<"Alert">> => unicode:characters_to_binary("用户名验证失败！", utf8)}]}
    end.

%% @doc
%% 验证登录名是否有重复(修改)
%% @end
validate_editloginname(UserId, LoginName) ->
    % io:format("UserId: ~p, LoginName: ~p~n", [UserId, LoginName]),
    AllowedChars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_-",
    Regex = "^[" ++ AllowedChars ++ "]+$",
    try
        case re:run(LoginName, Regex, [global, {capture, none}]) of
            match ->
                case erlang:byte_size(LoginName) of
                    L when L < 6 ->
                        {error, 1};
                    L when L > 18 ->
                        {error, 2};
                    _ ->
                        try
                            case eadm_pgpool:equery(pool_pg,
                                "select 1 from eadm_user where id != $1 and loginname = $2 and deleted is false;",
                                [UserId, LoginName]) of
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
        end
    catch
        _:Error ->
            lager:error("用户名验证失败：~p~n", [Error]),
            {json, [#{<<"Alert">> => unicode:characters_to_binary("用户名验证失败！", utf8)}]}
    end.

%% @doc
%% 验证二进制密码数据
%% @end
validate_password(PassWordBin) when erlang:is_binary(PassWordBin) ->
    PassWord = erlang:binary_to_list(PassWordBin),
    AllowedChars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789,\._-",
    Regex = "^[" ++ AllowedChars ++ "]+$",
    try
        case re:run(PassWord, Regex, [global, {capture, none}]) of
            match ->
                case erlang:byte_size(PassWordBin) of
                    L when L < 6 ->
                        {error, "密码不能少于6位！"};
                    L when L > 36 ->
                        {error, "密码不能大于36位！"};
                    _ ->
                        {ok}
                end;
            _ ->
                {error, "密码仅支持【英文、数字、符号：,._-】"}
        end
    catch
        _:Error ->
            lager:error("密码验证失败：~p~n", [Error]),
            {json, [#{<<"Alert">> => unicode:characters_to_binary("密码验证失败！", utf8)}]}
    end;
validate_password(_) ->
    {error, "密码格式错误！"}.

%% @doc
%% 获取用户权限
%% @end
get_permission(LoginName) ->
    try
        {ok, _, ResData} = eadm_pgpool:equery(pool_pg,
            "select rolepermission
            from vi_userpermission
            where loginname = $1
            limit 1;", [LoginName]),
        {ResBin} = hd(ResData),
        {ok, ResJson} = thoas:decode(ResBin),
        #{<<"data">> => ResJson}
    catch
        _:Error ->
            lager:error("权限获取失败：~p~n", [Error]),
            {json, [#{<<"Alert">> => unicode:characters_to_binary("权限获取失败！", utf8)}]}
    end.
