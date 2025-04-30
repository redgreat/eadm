%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2024, REDGREAT
%%% @doc
%%% 解析cowboy传过来的二进制数据
%%% 参考自: https://gitee.com/tercero/erlxlsx/blob/master/src/erlxlsx_worker.erl
%%% @end
%%% Created : 2024-03-19 09:47
%%%-------------------------------------------------------------------
-module(eadm_xlsx).
-author("wangcw").

%%%===================================================================
%%% 资源引用
%%%===================================================================
-include_lib("xmerl/include/xmerl.hrl").

%%%===================================================================
%%% 函数导出
%%%===================================================================
-export([load/1, load_sheet/2]).

%%====================================================================
%% API 函数
%%====================================================================
%% @doc
%% 加载XLSX文件，解压缩为内存中的二进制数据
%% @end
load(F) ->
    case zip:unzip(F, [memory]) of
        {ok, FileBin} ->
            Sheet1Only = application:get_env(erlxlsx, sheet1_only, true),
            load_sheet(FileBin, Sheet1Only);
        {error, Err} ->
            throw({F, Err})
    end.

%% @doc
%% 只读取第一个工作表(sheet1)，处理共享字符串和表格数据
%% @end
load_sheet(FileBin, true) ->
    Share = clean_share(proplists:get_value("xl/sharedStrings.xml", FileBin, undefined)),
    Sheet = clean_sheet(proplists:get_value("xl/worksheets/sheet1.xml", FileBin, undefined)),
    pack_table(Share, Sheet);

%% @doc
%% 读取所有工作表(sheet)，返回包含所有表格数据的列表
%% @end
load_sheet(FileBin, _) ->
    Share = clean_share(proplists:get_value("xl/sharedStrings.xml", FileBin, undefined)),
    Amount = 1,
    Tables = load_sheet(FileBin, Share, Amount, []),
    lists:reverse(Tables).

%% @doc
%% 处理共享字符串文件，提取所有共享字符串
%% @end
clean_share(undefined) -> [];
clean_share(Share) ->
    BinStr = application:get_env(erlxlsx, binary_string, true),
    {SST, _Rest} = xmerl_scan:string(binary_to_list(Share)),
    [binary_string(BinStr, lists:flatten(clean_share_content(SI))) || SI <- SST#xmlElement.content].

%% @doc
%% 处理表格数据，解析XML格式的工作表数据
%% @end
clean_sheet(undefined) -> [];
clean_sheet(Sheet) ->
    {Root, _Rest} = xmerl_scan:string(binary_to_list(Sheet)),
    SheetData = lists:keyfind(sheetData, #xmlElement.name, Root#xmlElement.content),
    [clean_sheet_row(Row) || Row <- SheetData#xmlElement.content, Row#xmlElement.name == row].

%% @doc
%% 将处理后的数据打包成表格格式
%% @end
pack_table(Share, Sheet) ->
    [pack_row(Row, Share) || Row <- Sheet].

%%====================================================================
%% 内部函数
%%====================================================================
%% @private
%% @doc
%% 递归读取所有工作表，一次处理一个工作表
%% @end
load_sheet(FileBin, Share, Amount, Tables) ->
    SheetName = "sheet" ++ integer_to_list(Amount),
    SheetPath = "xl/worksheets/" ++ SheetName ++ ".xml",
    case proplists:get_value(SheetPath, FileBin, undefined) of
        undefined -> Tables;
        SheetXML ->
            Sheet = clean_sheet(SheetXML),
            Table = {SheetName, pack_table(Share, Sheet)},
            load_sheet(FileBin, Share, Amount+1, [Table | Tables])
    end.

%% @private
%% @doc
%% 递归提取XML元素中的文本内容
%% @end
clean_share_content([]) -> [];
clean_share_content(#xmlText{value = Value}) -> Value;
clean_share_content(#xmlElement{content = Content}) -> clean_share_content(Content);
clean_share_content(L) -> [clean_share_content(T) || T <- L].

%% @private
%% @doc
%% 根据配置将字符串转换为二进制格式或保持原样
%% @end
binary_string(true, Str) -> unicode:characters_to_binary(Str);
binary_string(_, Str) -> Str.

%% @private
%% @doc
%% 处理表格行数据，提取行中的每个单元格
%% @end
clean_sheet_row(Row) ->
    [clean_sheet_c(C) || C <- Row#xmlElement.content, C#xmlElement.name == c].

%% @private
%% @doc
%% 处理单元格数据，提取单元格的类型和值
%% @end
clean_sheet_c(C) ->
    T = lists:keyfind(t, #xmlAttribute.name, C#xmlElement.attributes),
    V = lists:keyfind(v, #xmlElement.name,   C#xmlElement.content),
    clean_sheet_v(T, V).

%% @private
%% @doc
%% 处理单元格的值，如果没有内容则返回null
%% @end
clean_sheet_v(T, #xmlElement{content = [R]}) ->
    clean_sheet_v1(T, list_to_integer(R#xmlText.value));
clean_sheet_v(_, _) -> null.

%% @private
%% @doc
%% 处理单元格的值类型，如果是共享字符串则标记为需要转换
%% @end
clean_sheet_v1(#xmlAttribute{value = "s"}, V) -> {transform, V};
clean_sheet_v1(_, V) -> V.

%% @private
%% @doc
%% 处理表格中的一行数据
%% @end
pack_row(Row, Share) ->
    [pack_value(V, Share) || V <- Row].

%% @private
%% @doc
%% 处理单个单元格的值，如果需要转换则从共享字符串中获取
%% @end
pack_value({transform, V}, Share) -> lists:nth(V+1, Share);
pack_value(V, _Share) -> V.
