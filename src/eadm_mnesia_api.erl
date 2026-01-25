%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2026, REDGREAT
%%% @doc
%%% Mnesia操作API封装模块
%%% 提供统一的CRUD接口和查询功能
%%% @end
%%% Created : 2026-01-24 01:32:00
%%%-------------------------------------------------------------------
-module(eadm_mnesia_api).
-author("wangcw").

%%%===================================================================
%%% 头文件引用
%%%===================================================================
-include("eadm_mnesia.hrl").

%%%===================================================================
%%% 函数导出
%%%===================================================================
-export([create/2, read/2, update/3, delete/2, delete_hard/2]).
-export([query/2, query_all/1, count/1, count/2]).
-export([transaction/1, dirty_read/2, dirty_write/1]).
-export([get_next_id/1, find_by_field/3]).

%%%===================================================================
%%% API 函数
%%%===================================================================

%% @doc
%% 创建记录
%% @end
-spec create(Table :: atom(), Record :: tuple()) -> ok | {error, any()}.
create(Table, Record) ->
    transaction(fun() ->
        mnesia:write(Table, Record, write)
    end).

%% @doc
%% 读取记录
%% @end
-spec read(Table :: atom(), Key :: any()) -> [tuple()] | {error, any()}.
read(Table, Key) ->
    case mnesia:transaction(fun() ->
        mnesia:read(Table, Key)
    end) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

%% @doc
%% 更新记录
%% UpdateFun是一个接收旧记录并返回新记录的函数
%% @end
-spec update(Table :: atom(), Key :: any(), UpdateFun :: fun((tuple()) -> tuple())) -> 
    ok | {error, any()}.
update(Table, Key, UpdateFun) ->
    transaction(fun() ->
        case mnesia:read(Table, Key) of
            [] ->
                mnesia:abort({not_found, Key});
            [OldRecord] ->
                NewRecord = UpdateFun(OldRecord),
                mnesia:write(Table, NewRecord, write)
        end
    end).

%% @doc
%% 软删除记录(设置deleted=true)
%% @end
-spec delete(Table :: atom(), Key :: any()) -> ok | {error, any()}.
delete(Table, Key) ->
    transaction(fun() ->
        case mnesia:read(Table, Key) of
            [] ->
                mnesia:abort({not_found, Key});
            [Record] ->
                %% 假设record的倒数第一个字段是deleted
                %% 需要根据具体表调整
                DeletedRecord = set_deleted(Record, true),
                mnesia:write(Table, DeletedRecord, write)
        end
    end).

%% @doc
%% 硬删除记录(物理删除)
%% @end
-spec delete_hard(Table :: atom(), Key :: any()) -> ok | {error, any()}.
delete_hard(Table, Key) ->
    transaction(fun() ->
        mnesia:delete({Table, Key})
    end).

%% @doc
%% 查询记录
%% MatchSpec是mnesia的match specification格式
%% 示例: [{#eadm_user{deleted = false, _ = '_'}, [], ['$_']}]
%% @end
-spec query(Table :: atom(), MatchSpec :: list()) -> [tuple()] | {error, any()}.
query(Table, MatchSpec) ->
    case mnesia:transaction(fun() ->
        mnesia:select(Table, MatchSpec)
    end) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

%% @doc
%% 查询所有未删除的记录
%% @end
-spec query_all(Table :: atom()) -> [tuple()] | {error, any()}.
query_all(Table) ->
    case mnesia:transaction(fun() ->
        mnesia:foldl(fun(Record, Acc) ->
            case is_deleted(Record) of
                false -> [Record | Acc];
                true -> Acc
            end
        end, [], Table)
    end) of
        {atomic, Result} -> lists:reverse(Result);
        {aborted, Reason} -> {error, Reason}
    end.

%% @doc
%% 统计记录数
%% @end
-spec count(Table :: atom()) -> integer() | {error, any()}.
count(Table) ->
    case mnesia:transaction(fun() ->
        mnesia:table_info(Table, size)
    end) of
        {atomic, Count} -> Count;
        {aborted, Reason} -> {error, Reason}
    end.

%% @doc
%% 条件统计记录数
%% @end
-spec count(Table :: atom(), MatchSpec :: list()) -> integer() | {error, any()}.
count(Table, MatchSpec) ->
    case mnesia:transaction(fun() ->
        length(mnesia:select(Table, MatchSpec))
    end) of
        {atomic, Count} -> Count;
        {aborted, Reason} -> {error, Reason}
    end.

%% @doc
%% 事务包装
%% @end
-spec transaction(Fun :: fun(() -> any())) -> ok | {error, any()}.
transaction(Fun) ->
    case mnesia:transaction(Fun) of
        {atomic, ok} -> ok;
        {atomic, Result} -> {ok, Result};
        {aborted, Reason} -> {error, Reason}
    end.

%% @doc
%% 脏读(不使用事务,性能更高但不保证一致性)
%% @end
-spec dirty_read(Table :: atom(), Key :: any()) -> [tuple()].
dirty_read(Table, Key) ->
    mnesia:dirty_read(Table, Key).

%% @doc
%% 脏写(不使用事务)
%% @end
-spec dirty_write(Record :: tuple()) -> ok.
dirty_write(Record) ->
    mnesia:dirty_write(Record).

%% @doc
%% 获取下一个自增ID
%% 用于自增主键表(如eadm_userrole, eadm_dashboard等)
%% @end
-spec get_next_id(Table :: atom()) -> integer().
get_next_id(Table) ->
    %% 对于使用integer主键的表(userrole, dashboard, userdevice)
    %% 直接遍历所有记录找最大ID
    AllRecords = mnesia:dirty_match_object(Table, mnesia:table_info(Table, wild_pattern)),
    case AllRecords of
        [] ->
            1;
        _ ->
            MaxId = lists:foldl(fun(Record, Max) ->
                RecordName = element(1, Record),
                Id = case RecordName of
                    eadm_userrole -> (Record)#eadm_userrole.id;
                    eadm_dashboard -> (Record)#eadm_dashboard.id;
                    eadm_userdevice -> (Record)#eadm_userdevice.id;
                    _ -> 0
                end,
                if Id > Max -> Id; true -> Max end
            end, 0, AllRecords),
            MaxId + 1
    end.

%% @doc
%% 根据字段查找记录
%% @end
-spec find_by_field(Table :: atom(), FieldName :: atom(), Value :: any()) -> 
    [tuple()] | {error, any()}.
find_by_field(Table, FieldName, Value) ->
    case mnesia:transaction(fun() ->
        mnesia:index_read(Table, Value, FieldName)
    end) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

%%%===================================================================
%%% 内部函数
%%%===================================================================

%% @private
%% 设置记录的deleted字段为true
%% 这里需要根据不同的record类型进行处理
-spec set_deleted(Record :: tuple(), NewDeleted :: boolean()) -> tuple().
set_deleted(Record, NewDeleted) ->
    case element(1, Record) of
        eadm_tenant ->
            Record#eadm_tenant{deleted = NewDeleted, deletedat = erlang:system_time(second)};
        eadm_user ->
            Record#eadm_user{deleted = NewDeleted, deletedat = erlang:system_time(second)};
        eadm_role ->
            Record#eadm_role{deleted = NewDeleted, deletedat = erlang:system_time(second)};
        eadm_userrole ->
            Record#eadm_userrole{deleted = NewDeleted, deletedat = erlang:system_time(second)};
        eadm_crontab ->
            Record#eadm_crontab{deleted = NewDeleted, deletedat = erlang:system_time(second)};
        eadm_device ->
            Record#eadm_device{deleted = NewDeleted, deletedat = erlang:system_time(second)};
        eadm_userdevice ->
            Record#eadm_userdevice{deleted = NewDeleted, deletedat = erlang:system_time(second)};
        _ ->
            Record
    end.

%% @private
%% 检查记录是否已删除
-spec is_deleted(Record :: tuple()) -> boolean().
is_deleted(Record) ->
    %% 大部分表的deleted字段是倒数第一个
    %% eadm_dashboard没有deleted字段
    case element(1, Record) of
        eadm_dashboard ->
            false;
        _ ->
            Size = tuple_size(Record),
            element(Size, Record) =:= true
    end.
