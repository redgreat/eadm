%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2024, REDGREAT
%%% @doc
%%% 设备管理控制器
%%% @end
%%% Created : 2024-07-01
%%%-------------------------------------------------------------------
-module(eadm_device_controller).

-export([
    index/1,
    search/1,
    add/1,
    edit/1,
    delete/1,
    toggle_status/1,  % 新增启用/禁用设备API
    assign/1,
    unassign/1,
    device_users/1,
    user_devices/1
]).

%% @doc
%% 设备管理页面
%% @end
index(#{auth_data := #{<<"authed">> := true, <<"username">> := Username}}) ->
    {ok, [{username, Username}], #{view => eadm_device}};

index(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 查询设备列表
%% @end
search(#{auth_data := #{<<"authed">> := true,
      <<"permission">> := #{<<"device">> := #{<<"devlist">> := true}}},
    parsed_qs := #{<<"deviceNo">> := DeviceNo}}) ->
    try
        {ok, Columns, ResData} = eadm_pgpool:equery(pool_pg,
            "select deviceno, imei, simno, remark, enable, createdat
            from eadm_device
            where deviceno like $1
              and deleted is false
            order by createdat desc;",
            [<<"%", DeviceNo/binary, "%">>]),
        {json, eadm_utils:pg_as_json(Columns, ResData)}
    catch
        _:Error ->
            lager:info("设备查询失败：~p", [Error]),
            {json, [#{<<"Alert">> => unicode:characters_to_binary("设备查询失败！", utf8)}]}
    end;

search(#{auth_data := #{<<"authed">> := true,
      <<"permission">> := #{<<"device">> := #{<<"devlist">> := true}}},
    parsed_qs := _}) ->
    try
        {ok, Columns, ResData} = eadm_pgpool:equery(pool_pg,
            "select deviceno, imei, simno, remark, enable, createdat
            from eadm_device
            where deleted is false
            order by createdat desc;",
            []),
        {json, eadm_utils:pg_as_json(Columns, ResData)}
    catch
        _:Error ->
            lager:info("设备查询失败：~p", [Error]),
            {json, [#{<<"Alert">> => unicode:characters_to_binary("设备查询失败！", utf8)}]}
    end;

search(#{auth_data := #{<<"permission">> := #{<<"device">> := #{<<"devlist">> := false}}}}) ->
    {json, [#{<<"Alert">> => unicode:characters_to_binary("API鉴权失败！", utf8)}]};

search(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 添加设备
%% @end
add(#{auth_data := #{<<"authed">> := true, <<"loginname">> := LoginName,
      <<"permission">> := #{<<"device">> := #{<<"devadd">> := true}}},
    json := #{<<"deviceNo">> := DeviceNo, <<"imei">> := Imei, <<"simNo">> := SimNo, <<"remark">> := Remark}}) ->
    try
        % 检查设备号是否已存在
        {ok, _, ExistData} = eadm_pgpool:equery(pool_pg,
            "select count(*) from eadm_device where deviceno = $1 and deleted is false;",
            [DeviceNo]),
        case ExistData of
            [{0}] ->
                % 设备不存在，可以添加
                % 使用匹配任意返回值的模式，因为插入操作可能返回 {ok, RowCount} 或 {ok, _, _}
                {ok, _} = eadm_pgpool:equery(pool_pg,
                    "insert into eadm_device(deviceno, imei, simno, remark, createduser, updateduser)
                    values($1, $2, $3, $4, $5, $6);",
                    [DeviceNo, Imei, SimNo, Remark, LoginName, LoginName]),
                {json, [#{<<"Alert">> => unicode:characters_to_binary("设备添加成功！", utf8)}]};
            _ ->
                % 设备已存在
                {json, [#{<<"Alert">> => unicode:characters_to_binary("设备号已存在！", utf8)}]}
        end
    catch
        _:Error ->
            lager:info("设备添加失败：~p", [Error]),
            {json, [#{<<"Alert">> => unicode:characters_to_binary("设备添加失败！", utf8)}]}
    end;

add(#{auth_data := #{<<"permission">> := #{<<"device">> := #{<<"devadd">> := false}}}}) ->
    {json, [#{<<"Alert">> => unicode:characters_to_binary("API鉴权失败！", utf8)}]};

add(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 编辑设备
%% @end
edit(#{auth_data := #{<<"authed">> := true, <<"loginname">> := LoginName,
      <<"permission">> := #{<<"device">> := #{<<"devedit">> := true}}},
    json := #{<<"deviceNo">> := DeviceNo, <<"imei">> := Imei, <<"simNo">> := SimNo, <<"remark">> := Remark, <<"enable">> := Enable}}) ->
    try
        % 检查设备是否存在
        {ok, _, ExistData} = eadm_pgpool:equery(pool_pg,
            "select count(*) from eadm_device where deviceno = $1 and deleted is false;",
            [DeviceNo]),
        case ExistData of
            [{1}] ->
                % 设备存在，可以更新
                % 使用匹配任意返回值的模式，因为更新操作可能返回 {ok, RowCount} 或 {ok, _, _}
                {ok, _} = eadm_pgpool:equery(pool_pg,
                    "update eadm_device set imei = $1, simno = $2, remark = $3, enable = $4, updateduser = $5
                    where deviceno = $6 and deleted is false;",
                    [Imei, SimNo, Remark, Enable, LoginName, DeviceNo]),
                {json, [#{<<"Alert">> => unicode:characters_to_binary("设备更新成功！", utf8)}]};
            _ ->
                % 设备不存在
                {json, [#{<<"Alert">> => unicode:characters_to_binary("设备不存在！", utf8)}]}
        end
    catch
        _:Error ->
            lager:info("设备更新失败：~p", [Error]),
            {json, [#{<<"Alert">> => unicode:characters_to_binary("设备更新失败！", utf8)}]}
    end;

edit(#{auth_data := #{<<"permission">> := #{<<"device">> := #{<<"devedit">> := false}}}}) ->
    {json, [#{<<"Alert">> => unicode:characters_to_binary("API鉴权失败！", utf8)}]};

edit(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 删除设备
%% @end
delete(#{auth_data := #{<<"authed">> := true, <<"loginname">> := LoginName,
      <<"permission">> := #{<<"device">> := #{<<"devdel">> := true}}},
    bindings := #{<<"deviceNo">> := DeviceNo}}) ->
    try
        % 检查设备是否存在
        {ok, _, ExistData} = eadm_pgpool:equery(pool_pg,
            "select count(*) from eadm_device where deviceno = $1 and deleted is false;",
            [DeviceNo]),
        case ExistData of
            [{1}] ->
                % 设备存在，可以删除
                % 使用匹配任意返回值的模式，因为更新操作可能返回 {ok, RowCount} 或 {ok, _, _}
                {ok, _} = eadm_pgpool:equery(pool_pg,
                    "update eadm_device set deleted = true, deleteduser = $1, deletedat = current_timestamp
                    where deviceno = $2 and deleted is false;",
                    [LoginName, DeviceNo]),
                {json, [#{<<"Alert">> => unicode:characters_to_binary("设备删除成功！", utf8)}]};
            _ ->
                % 设备不存在
                {json, [#{<<"Alert">> => unicode:characters_to_binary("设备不存在！", utf8)}]}
        end
    catch
        _:Error ->
            lager:info("设备删除失败：~p", [Error]),
            {json, [#{<<"Alert">> => unicode:characters_to_binary("设备删除失败！", utf8)}]}
    end;

delete(#{auth_data := #{<<"permission">> := #{<<"device">> := #{<<"devdel">> := false}}}}) ->
    {json, [#{<<"Alert">> => unicode:characters_to_binary("API鉴权失败！", utf8)}]};

delete(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 启用/禁用设备
%% @end
toggle_status(#{auth_data := #{<<"authed">> := true, <<"loginname">> := LoginName,
      <<"permission">> := #{<<"device">> := #{<<"devedit">> := true}}},
    json := #{<<"deviceNo">> := DeviceNo}}) ->
    try
        % 检查设备是否存在
        {ok, _, ExistData} = eadm_pgpool:equery(pool_pg,
            "select enable from eadm_device where deviceno = $1 and deleted is false;",
            [DeviceNo]),
        case ExistData of
            [{CurrentStatus}] ->
                % 设备存在，切换状态
                NewStatus = not CurrentStatus,
                % 使用匹配任意返回值的模式，因为更新操作可能返回 {ok, RowCount} 或 {ok, _, _}
                case eadm_pgpool:equery(pool_pg,
                    "update eadm_device set enable = $1, updateduser = $2, updatedat = current_timestamp
                    where deviceno = $3 and deleted is false;",
                    [NewStatus, LoginName, DeviceNo]) of
                    {ok, _} ->
                        StatusText = case NewStatus of
                            true -> "启用";
                            false -> "禁用"
                        end,
                        {json, [#{<<"Alert">> => unicode:characters_to_binary("设备" ++ StatusText ++ "成功！", utf8)}]};
                    {error, _Reason} ->
                        {json, [#{<<"Alert">> => unicode:characters_to_binary("设备状态更新失败：数据库操作错误", utf8)}]}
                end;
            [] ->
                % 设备不存在
                {json, [#{<<"Alert">> => unicode:characters_to_binary("设备不存在或已被删除！", utf8)}]};
            _ ->
                % 其他异常情况
                {json, [#{<<"Alert">> => unicode:characters_to_binary("查询设备状态失败：数据格式异常", utf8)}]}
        end
    catch
        error:no_connection ->
            {json, [#{<<"Alert">> => unicode:characters_to_binary("设备状态切换失败：数据库连接失败", utf8)}]};
        error:{badmatch, {error, _QueryError}} ->
            {json, [#{<<"Alert">> => unicode:characters_to_binary("设备状态切换失败：数据库查询错误", utf8)}]};
        _:_Error ->
            {json, [#{<<"Alert">> => unicode:characters_to_binary("设备状态切换失败：系统内部错误", utf8)}]}
    end;

toggle_status(#{auth_data := #{<<"permission">> := #{<<"device">> := #{<<"devedit">> := false}}}, json := _}) ->
    {json, [#{<<"Alert">> => unicode:characters_to_binary("API鉴权失败：您没有编辑设备的权限！", utf8)}]};

toggle_status(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"};

% 处理缺少设备号的情况
toggle_status(#{auth_data := #{<<"authed">> := true}, json := Json}) ->
    case maps:is_key(<<"deviceNo">>, Json) of
        false ->
            {json, [#{<<"Alert">> => unicode:characters_to_binary("请求参数错误：缺少设备号", utf8)}]};
        true ->
            % 这种情况不应该发生，因为前面的模式匹配应该已经处理了有效的请求
            {json, [#{<<"Alert">> => unicode:characters_to_binary("请求处理失败：内部错误", utf8)}]}
    end.

%% @doc
%% 分配设备给用户
%% @end
assign(#{auth_data := #{<<"authed">> := true, <<"loginname">> := LoginName,
      <<"permission">> := #{<<"device">> := #{<<"devassign">> := true}}},
    json := #{<<"deviceNo">> := DeviceNo, <<"userId">> := UserId, <<"userLoginName">> := UserLoginName}}) ->
    try
        % 检查设备是否已分配给该用户
        {ok, _, ExistData} = eadm_pgpool:equery(pool_pg,
            "select count(*) from eadm_userdevice
            where deviceno = $1 and userid = $2 and loginname = $3 and deleted is false;",
            [DeviceNo, UserId, UserLoginName]),
        case ExistData of
            [{0}] ->
                % 未分配，可以添加
                % 使用匹配任意返回值的模式，因为插入操作可能返回 {ok, RowCount} 或 {ok, _, _}
                {ok, _} = eadm_pgpool:equery(pool_pg,
                    "insert into eadm_userdevice(userid, loginname, deviceno, createduser, updateduser)
                    values($1, $2, $3, $4, $5);",
                    [UserId, UserLoginName, DeviceNo, LoginName, LoginName]),
                {json, [#{<<"Alert">> => unicode:characters_to_binary("设备分配成功！", utf8)}]};
            _ ->
                % 已分配
                {json, [#{<<"Alert">> => unicode:characters_to_binary("设备已分配给该用户！", utf8)}]}
        end
    catch
        _:Error ->
            lager:info("设备分配失败：~p", [Error]),
            {json, [#{<<"Alert">> => unicode:characters_to_binary("设备分配失败！", utf8)}]}
    end;

assign(#{auth_data := #{<<"permission">> := #{<<"device">> := #{<<"devassign">> := false}}}}) ->
    {json, [#{<<"Alert">> => unicode:characters_to_binary("API鉴权失败！", utf8)}]};

assign(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 取消设备分配
%% @end
unassign(#{auth_data := #{<<"authed">> := true, <<"loginname">> := LoginName,
      <<"permission">> := #{<<"device">> := #{<<"devassign">> := true}}},
    bindings := #{<<"id">> := Id}}) ->
    try
        % 确保Id是整数
        IdInt = case is_binary(Id) of
            true ->
                try
                    binary_to_integer(Id)
                catch
                    _:_ ->
                        lager:info("ID转换失败，非法ID值: ~p", [Id]),
                        throw({invalid_id, Id})
                end;
            false -> Id
        end,

        % 检查分配记录是否存在
        {ok, _, ExistData} = eadm_pgpool:equery(pool_pg,
            "select count(*) from eadm_userdevice where id = $1 and deleted is false;",
            [IdInt]),
        case ExistData of
            [{1}] ->
                % 记录存在，可以删除
                % 使用匹配任意返回值的模式，因为更新操作可能返回 {ok, RowCount} 或 {ok, _, _}
                {ok, _} = eadm_pgpool:equery(pool_pg,
                    "update eadm_userdevice set deleted = true, deleteduser = $1, deletedat = current_timestamp
                    where id = $2 and deleted is false;",
                    [LoginName, IdInt]),
                {json, [#{<<"Alert">> => unicode:characters_to_binary("设备取消分配成功！", utf8)}]};
            _ ->
                % 记录不存在
                {json, [#{<<"Alert">> => unicode:characters_to_binary("分配记录不存在！", utf8)}]}
        end
    catch
        _:{invalid_id, InvalidId} ->
            lager:info("设备取消分配失败：无效的ID值 ~p", [InvalidId]),
            {json, [#{<<"Alert">> => unicode:characters_to_binary("无效的记录ID！", utf8)}]};
        _:Error ->
            lager:info("设备取消分配失败：~p", [Error]),
            {json, [#{<<"Alert">> => unicode:characters_to_binary("设备取消分配失败！", utf8)}]}
    end;

unassign(#{auth_data := #{<<"permission">> := #{<<"device">> := #{<<"devassign">> := false}}}}) ->
    {json, [#{<<"Alert">> => unicode:characters_to_binary("API鉴权失败！", utf8)}]};

unassign(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 获取设备分配的用户列表
%% @end
device_users(#{auth_data := #{<<"authed">> := true,
      <<"permission">> := #{<<"device">> := #{<<"devlist">> := true}}},
    bindings := #{<<"deviceNo">> := DeviceNo}}) ->
    try
        {ok, Columns, ResData} = eadm_pgpool:equery(pool_pg,
            "select ud.id, ud.userid, ud.loginname, u.username
            from eadm_userdevice ud
            join eadm_user u on ud.userid = u.id and ud.loginname = u.loginname
            where ud.deviceno = $1 and ud.deleted is false
            order by ud.createdat desc;",
            [DeviceNo]),
        {json, eadm_utils:pg_as_json(Columns, ResData)}
    catch
        _:Error ->
            lager:info("设备用户查询失败：~p", [Error]),
            {json, [#{<<"Alert">> => unicode:characters_to_binary("设备用户查询失败！", utf8)}]}
    end;

device_users(#{auth_data := #{<<"permission">> := #{<<"device">> := #{<<"devlist">> := false}}}}) ->
    {json, [#{<<"Alert">> => unicode:characters_to_binary("API鉴权失败！", utf8)}]};

device_users(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 获取用户可访问的设备列表
%% @end
user_devices(#{auth_data := #{<<"authed">> := true, <<"loginname">> := LoginName,
      <<"permission">> := #{<<"locate">> := true}}}) ->
    try
        {ok, Columns, ResData} = eadm_pgpool:equery(pool_pg,
            "select d.deviceno, d.imei, d.remark
            from eadm_device d
            join eadm_userdevice ud on d.deviceno = ud.deviceno
            where ud.loginname = $1
              and ud.deleted is false
              and d.deleted is false
              and d.enable is true
            order by d.createdat desc;",
            [LoginName]),
        {json, eadm_utils:pg_as_json(Columns, ResData)}
    catch
        _:Error ->
            lager:info("用户设备查询失败：~p", [Error]),
            {json, [#{<<"Alert">> => unicode:characters_to_binary("用户设备查询失败！", utf8)}]}
    end;

user_devices(#{auth_data := #{<<"permission">> := #{<<"locate">> := false}}}) ->
    {json, [#{<<"Alert">> => unicode:characters_to_binary("API鉴权失败！", utf8)}]};

user_devices(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.
