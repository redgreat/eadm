%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2026, REDGREAT
%%% @doc
%%% Mnesia备份恢复模块
%%% 提供完整备份、增量备份、恢复、定时备份等功能
%%% @end
%%% Created : 2026-01-24 01:33:00
%%%-------------------------------------------------------------------
-module(eadm_mnesia_backup).
-author("wangcw").

%%%===================================================================
%%% 函数导出
%%%===================================================================
-export([backup/0, backup/1, backup_compressed/0, backup_compressed/1]).
-export([restore/1, restore_with_validation/1]).
-export([cleanup_old_backups/1, list_backups/0]).
-export([scheduled_backup/0, init_backup_scheduler/0]).

%%%===================================================================
%%% 宏定义
%%%===================================================================
-define(BACKUP_DIR, "/opt/eadm/backups").
-define(BACKUP_EXT, ".bup").
-define(BACKUP_GZ_EXT, ".bup.gz").
-define(RETENTION_DAYS, 30).

%%%===================================================================
%%% API 函数
%%%===================================================================

%% @doc
%% 完整备份到默认路径
%% 备份文件名格式: mnesia_backup_YYYYMMDD_HHMMSS.bup
%% @end
-spec backup() -> {ok, string()} | {error, any()}.
backup() ->
    BackupFile = generate_backup_filename(),
    backup(BackupFile).

%% @doc
%% 完整备份到指定路径
%% @end
-spec backup(BackupFile :: string() | binary()) -> {ok, string()} | {error, any()}.
backup(BackupFile) when is_binary(BackupFile) ->
    backup(binary_to_list(BackupFile));
backup(BackupFile) when is_list(BackupFile) ->
    %% 确保备份目录存在
    ensure_backup_dir(),

    %% 执行备份
    case mnesia:backup(BackupFile) of
        ok ->
            lager:info("Mnesia备份成功: ~s", [BackupFile]),
            {ok, BackupFile};
        {error, Reason} ->
            lager:error("Mnesia备份失败: ~p", [Reason]),
            {error, Reason}
    end.

%% @doc
%% 备份并压缩到默认路径
%% @end
-spec backup_compressed() -> {ok, string()} | {error, any()}.
backup_compressed() ->
    BackupFile = generate_backup_filename(),
    backup_compressed(BackupFile).

%% @doc
%% 备份并压缩到指定路径
%% @end
-spec backup_compressed(BackupFile :: string() | binary()) -> {ok, string()} | {error, any()}.
backup_compressed(BackupFile) when is_binary(BackupFile) ->
    backup_compressed(binary_to_list(BackupFile));
backup_compressed(BackupFile) when is_list(BackupFile) ->
    %% 先执行正常备份
    case backup(BackupFile) of
        {ok, BupFile} ->
            %% 压缩备份文件
            GzFile = BupFile ++ ".gz",
            case compress_file(BupFile, GzFile) of
                ok ->
                    %% 删除未压缩的备份文件
                    file:delete(BupFile),
                    lager:info("Mnesia备份压缩成功: ~s", [GzFile]),
                    {ok, GzFile};
                {error, Reason} ->
                    lager:error("备份压缩失败: ~p", [Reason]),
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc
%% 从备份恢复
%% @end
-spec restore(BackupFile :: string() | binary()) -> ok | {error, any()}.
restore(BackupFile) when is_binary(BackupFile) ->
    restore(binary_to_list(BackupFile));
restore(BackupFile) when is_list(BackupFile) ->
    %% 检查文件是否存在
    case filelib:is_file(BackupFile) of
        false ->
            {error, {file_not_found, BackupFile}};
        true ->
            %% 如果是压缩文件,先解压
            ActualFile =
                case filename:extension(BackupFile) of
                    ".gz" ->
                        DecompFile = filename:rootname(BackupFile),
                        case decompress_file(BackupFile, DecompFile) of
                            ok ->
                                DecompFile;
                            {error, Reason} ->
                                lager:error("解压备份文件失败: ~p", [Reason]),
                                throw({error, {decompress_failed, Reason}})
                        end;
                    _ ->
                        BackupFile
                end,

            %% 执行恢复
            RestoreResult =
                try
                    %% 停止mnesia
                    lager:info("停止Mnesia准备恢复..."),
                    mnesia:stop(),

                    %% 安装fallback
                    case mnesia:install_fallback(ActualFile) of
                        ok ->
                            lager:info("Fallback安装成功,启动Mnesia..."),

                            %% 启动mnesia
                            mnesia:start(),

                            %% 等待表就绪
                            Tables = eadm_mnesia:get_all_tables(),
                            case mnesia:wait_for_tables(Tables, 30000) of
                                ok ->
                                    lager:info("Mnesia恢复成功"),
                                    ok;
                                {timeout, BadTables} ->
                                    lager:error("恢复后等待表超时: ~p", [BadTables]),
                                    {error, {timeout, BadTables}};
                                {error, WaitReason} ->
                                    lager:error("恢复后等待表失败: ~p", [WaitReason]),
                                    {error, WaitReason}
                            end;
                        {error, FallbackReason} ->
                            lager:error("Fallback安装失败: ~p", [FallbackReason]),
                            mnesia:start(),
                            {error, FallbackReason}
                    end
                catch
                    Type:Error ->
                        lager:error("恢复过程异常: ~p:~p", [Type, Error]),
                        mnesia:start(),
                        {error, {restore_failed, Error}}
                end,
            RestoreResult
    end.

%% @doc
%% 恢复并验证数据完整性
%% @end
-spec restore_with_validation(BackupFile :: string() | binary()) -> ok | {error, any()}.
restore_with_validation(BackupFile) ->
    case restore(BackupFile) of
        ok ->
            %% 验证数据完整性
            case validate_data() of
                ok ->
                    lager:info("数据恢复并验证成功"),
                    ok;
                {error, Reason} ->
                    lager:error("数据验证失败: ~p", [Reason]),
                    {error, {validation_failed, Reason}}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc
%% 清理N天前的备份文件
%% @end
-spec cleanup_old_backups(Days :: integer()) -> ok.
cleanup_old_backups(Days) ->
    ensure_backup_dir(),

    %% 获取所有备份文件
    Pattern = filename:join(?BACKUP_DIR, "mnesia_backup_*"),
    Files = filelib:wildcard(Pattern),

    %% 获取N天前的时间戳
    Now = erlang:system_time(second),
    Threshold = Now - (Days * 24 * 3600),

    %% 删除超过N天的文件
    lists:foreach(
        fun(File) ->
            case file:read_file_info(File) of
                {ok, FileInfo} ->
                    %% FileInfo是一个record,mtime字段位置是第5个
                    MTime = calendar:datetime_to_gregorian_seconds(element(5, FileInfo)),
                    %% 转换为Unix时间戳
                    UnixTime = MTime - 62167219200,

                    if
                        UnixTime < Threshold ->
                            lager:info("删除旧备份文件: ~s", [File]),
                            file:delete(File);
                        true ->
                            ok
                    end;
                {error, _} ->
                    ok
            end
        end,
        Files
    ),

    lager:info("备份文件清理完成,保留最近~p天", [Days]),
    ok.

%% @doc
%% 列出所有备份文件
%% @end
-spec list_backups() -> [string()].
list_backups() ->
    ensure_backup_dir(),
    Pattern = filename:join(?BACKUP_DIR, "mnesia_backup_*"),
    filelib:wildcard(Pattern).

%% @doc
%% 定时备份任务(每日凌晨3点执行)
%% @end
-spec scheduled_backup() -> ok.
scheduled_backup() ->
    try
        lager:info("开始执行定时备份..."),

        %% 执行压缩备份
        case backup_compressed() of
            {ok, BackupFile} ->
                lager:info("定时备份成功: ~s", [BackupFile]),

                %% 清理超过30天的备份
                cleanup_old_backups(?RETENTION_DAYS),
                ok;
            {error, Reason} ->
                lager:error("定时备份失败: ~p", [Reason]),
                {error, Reason}
        end
    catch
        Type:Error ->
            lager:error("定时备份异常: ~p:~p", [Type, Error]),
            {error, {backup_exception, Error}}
    end.

%% @doc
%% 初始化备份调度器
%% 在eadm_crontab_controller中调用
%% @end
-spec init_backup_scheduler() -> ok.
init_backup_scheduler() ->
    %% 这个函数会在eadm_crontab_controller中注册定时任务
    %% cronexp: "0 3 * * *" (每天凌晨3点)
    %% cronmfa: "eadm_mnesia_backup:scheduled_backup/0"
    lager:info("Mnesia备份调度器已初始化"),
    ok.

%%%===================================================================
%%% 内部函数
%%%===================================================================

%% @private
%% 生成备份文件名
-spec generate_backup_filename() -> string().
generate_backup_filename() ->
    {{Y, M, D}, {H, Mi, S}} = calendar:local_time(),
    Filename = io_lib:format(
        "mnesia_backup_~4..0B~2..0B~2..0B_~2..0B~2..0B~2..0B~s",
        [Y, M, D, H, Mi, S, ?BACKUP_EXT]
    ),
    filename:join(?BACKUP_DIR, lists:flatten(Filename)).

%% @private
%% 确保备份目录存在
-spec ensure_backup_dir() -> ok.
ensure_backup_dir() ->
    case filelib:is_dir(?BACKUP_DIR) of
        true ->
            ok;
        false ->
            lager:info("创建备份目录: ~s", [?BACKUP_DIR]),
            filelib:ensure_dir(filename:join(?BACKUP_DIR, "dummy")),
            ok
    end.

%% @private
%% 压缩文件
-spec compress_file(SourceFile :: string(), GzFile :: string()) -> ok | {error, any()}.
compress_file(SourceFile, GzFile) ->
    try
        {ok, Data} = file:read_file(SourceFile),
        Compressed = zlib:gzip(Data),
        file:write_file(GzFile, Compressed)
    catch
        Type:Error ->
            {error, {compress_failed, Type, Error}}
    end.

%% @private
%% 解压文件
-spec decompress_file(GzFile :: string(), TargetFile :: string()) -> ok | {error, any()}.
decompress_file(GzFile, TargetFile) ->
    try
        {ok, CompData} = file:read_file(GzFile),
        Decompressed = zlib:gunzip(CompData),
        file:write_file(TargetFile, Decompressed)
    catch
        Type:Error ->
            {error, {decompress_failed, Type, Error}}
    end.

%% @private
%% 验证数据完整性
-spec validate_data() -> ok | {error, any()}.
validate_data() ->
    try
        Tables = eadm_mnesia:get_all_tables(),

        %% 验证所有表都可访问
        lists:foreach(
            fun(Table) ->
                case mnesia:table_info(Table, size) of
                    Size when is_integer(Size) ->
                        lager:info("表 ~p 记录数: ~p", [Table, Size]),
                        ok;
                    _ ->
                        throw({invalid_table, Table})
                end
            end,
            Tables
        ),

        %% 验证关键数据完整性
        case mnesia:dirty_read(eadm_tenant, <<"et0000000001">>) of
            [_] ->
                lager:info("关键数据验证通过"),
                ok;
            [] ->
                lager:warning("未找到默认租户数据"),
                {error, missing_default_tenant}
        end
    catch
        throw:Reason ->
            {error, Reason};
        Type:Error ->
            {error, {validation_exception, Type, Error}}
    end.
