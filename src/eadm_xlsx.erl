%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2024, REDGREAT
%%% @doc
%%% 解析cowboy传过来的二进制数据
%%% Copy & Thanks : https://gitee.com/tercero/erlxlsx/blob/master/src/erlxlsx_worker.erl
%%% @end
%%% Created : 2024-03-19 09:47
%%%-------------------------------------------------------------------
-module(eadm_xlsx).
-author("wangcw").

-include_lib("xmerl/include/xmerl.hrl").

-export([load/1, load_sheet/2]).
%%%===================================================================
%%% Application callbacks
%%%===================================================================

%% --------------------------------------------------------------------
load(F) ->
    case zip:unzip(F, [memory]) of
        {ok, FileBin} ->
            Sheet1Only = application:get_env(erlxlsx, sheet1_only, true),
            load_sheet(FileBin, Sheet1Only);
        {error, Err} ->
            throw({F, Err})
    end.

%% 只读取 sheet1
load_sheet(FileBin, true) ->
    Share = clean_share(proplists:get_value("xl/sharedStrings.xml", FileBin, undefined)),
    Sheet = clean_sheet(proplists:get_value("xl/worksheets/sheet1.xml", FileBin, undefined)),
    pack_table(Share, Sheet);

%% 读取所有 sheet
load_sheet(FileBin, _) ->
    Share = clean_share(proplists:get_value("xl/sharedStrings.xml", FileBin, undefined)),
    Amount = 1,
    Tables = load_sheet(FileBin, Share, Amount, []),
    lists:reverse(Tables).

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


%% --------------------------------------------------------------------
%% 共享字符
clean_share(undefined) -> [];
clean_share(Share) ->
    BinStr = application:get_env(erlxlsx, binary_string, true),
    {SST, _Rest} = xmerl_scan:string(binary_to_list(Share)),
    [binary_string(BinStr, lists:flatten(clean_share_content(SI))) || SI <- SST#xmlElement.content].

clean_share_content([]) -> [];
clean_share_content(#xmlText{value = Value}) -> Value;
clean_share_content(#xmlElement{content = Content}) -> clean_share_content(Content);
clean_share_content(L) -> [clean_share_content(T) || T <- L].

binary_string(true, Str) -> unicode:characters_to_binary(Str);
binary_string(_, Str) -> Str.


%% --------------------------------------------------------------------
%% 表格数据
clean_sheet(undefined) -> [];
clean_sheet(Sheet) ->
    {Root, _Rest} = xmerl_scan:string(binary_to_list(Sheet)),
    SheetData = lists:keyfind(sheetData, #xmlElement.name, Root#xmlElement.content),
    [clean_sheet_row(Row) || Row <- SheetData#xmlElement.content, Row#xmlElement.name == row].

clean_sheet_row(Row) ->
    [clean_sheet_c(C) || C <- Row#xmlElement.content, C#xmlElement.name == c].

clean_sheet_c(C) ->
    T = lists:keyfind(t, #xmlAttribute.name, C#xmlElement.attributes),
    V = lists:keyfind(v, #xmlElement.name,   C#xmlElement.content),
    clean_sheet_v(T, V).

%% 表格没有内容，为 null
clean_sheet_v(T, #xmlElement{content = [R]}) ->
    clean_sheet_v1(T, list_to_integer(R#xmlText.value));
clean_sheet_v(_, _) -> null.

%% 表格数据为字符串，需要从 share 转换
clean_sheet_v1(#xmlAttribute{value = "s"}, V) -> {transform, V};
clean_sheet_v1(_, V) -> V.


%% --------------------------------------------------------------------
%% 数据打包成表
pack_table(Share, Sheet) ->
    [pack_row(Row, Share) || Row <- Sheet].

pack_row(Row, Share) ->
    [pack_value(V, Share) || V <- Row].

pack_value({transform, V}, Share) -> lists:nth(V+1, Share);
pack_value(V, _Share) -> V.
