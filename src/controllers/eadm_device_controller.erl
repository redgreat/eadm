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
    assign/1,
    unassign/1,
    device_users/1,
    user_devices/1
]).

-include_lib("kernel/include/logger.hrl").

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
            ?LOG_ERROR("设备查询失败：~p~n", [Error]),
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
            ?LOG_ERROR("设备查询失败：~p~n", [Error]),
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
                {ok, _, _} = eadm_pgpool:equery(pool_pg,
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
            ?LOG_ERROR("设备添加失败：~p~n", [Error]),
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
                {ok, _, _} = eadm_pgpool:equery(pool_pg,
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
            ?LOG_ERROR("设备更新失败：~p~n", [Error]),
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
                {ok, _, _} = eadm_pgpool:equery(pool_pg,
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
            ?LOG_ERROR("设备删除失败：~p~n", [Error]),
            {json, [#{<<"Alert">> => unicode:characters_to_binary("设备删除失败！", utf8)}]}
    end;

delete(#{auth_data := #{<<"permission">> := #{<<"device">> := #{<<"devdel">> := false}}}}) ->
    {json, [#{<<"Alert">> => unicode:characters_to_binary("API鉴权失败！", utf8)}]};

delete(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

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
                {ok, _, _} = eadm_pgpool:equery(pool_pg,
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
            ?LOG_ERROR("设备分配失败：~p~n", [Error]),
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
        % 检查分配记录是否存在
        {ok, _, ExistData} = eadm_pgpool:equery(pool_pg,
            "select count(*) from eadm_userdevice where id = $1 and deleted is false;",
            [Id]),
        case ExistData of
            [{1}] ->
                % 记录存在，可以删除
                {ok, _, _} = eadm_pgpool:equery(pool_pg,
                    "update eadm_userdevice set deleted = true, deleteduser = $1, deletedat = current_timestamp
                    where id = $2 and deleted is false;",
                    [LoginName, Id]),
                {json, [#{<<"Alert">> => unicode:characters_to_binary("设备取消分配成功！", utf8)}]};
            _ ->
                % 记录不存在
                {json, [#{<<"Alert">> => unicode:characters_to_binary("分配记录不存在！", utf8)}]}
        end
    catch
        _:Error ->
            ?LOG_ERROR("设备取消分配失败：~p~n", [Error]),
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
            ?LOG_ERROR("设备用户查询失败：~p~n", [Error]),
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
            ?LOG_ERROR("用户设备查询失败：~p~n", [Error]),
            {json, [#{<<"Alert">> => unicode:characters_to_binary("用户设备查询失败！", utf8)}]}
    end;

user_devices(#{auth_data := #{<<"permission">> := #{<<"locate">> := false}}}) ->
    {json, [#{<<"Alert">> => unicode:characters_to_binary("API鉴权失败！", utf8)}]};

user_devices(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.
