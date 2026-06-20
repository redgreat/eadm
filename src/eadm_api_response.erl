%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2024, REDGREAT
%%% @doc
%%%  API response helpers shared by Nova controllers and Cowboy handlers.
%%% @end
%%%-------------------------------------------------------------------
-module(eadm_api_response).
-author("wangcw").
-compile({no_auto_import, [error/2, error/3]}).

-export([
    ok/0,
    ok/1,
    ok/2,
    error/1,
    error/2,
    error/3,
    validation_error/1,
    unauthorized/0,
    forbidden/0,
    not_found/0,
    nova_json/1,
    cowboy_json/2,
    cowboy_json/3
]).

%%====================================================================
%% API functions
%%====================================================================

ok() ->
    ok(#{}, <<"">>).

ok(Data) ->
    ok(Data, <<"">>).

ok(Data, Message) ->
    #{
        <<"success">> => true,
        <<"code">> => <<"ok">>,
        <<"message">> => to_binary(Message),
        <<"data">> => Data
    }.

error(Message) ->
    error(<<"error">>, Message, #{}).

error(Code, Message) ->
    error(Code, Message, #{}).

error(Code, Message, Details) ->
    #{
        <<"success">> => false,
        <<"code">> => to_binary(Code),
        <<"message">> => to_binary(Message),
        <<"data">> => Details
    }.

validation_error(Message) ->
    error(<<"validation_error">>, Message).

unauthorized() ->
    error(<<"unauthorized">>, <<"请先登录">>).

forbidden() ->
    error(<<"forbidden">>, <<"没有操作权限">>).

not_found() ->
    error(<<"not_found">>, <<"资源不存在">>).

nova_json(Body) ->
    {json, Body}.

cowboy_json(Req, Body) ->
    cowboy_json(Req, 200, Body).

cowboy_json(Req, Status, Body) ->
    Headers = #{
        <<"content-type">> => <<"application/json; charset=utf-8">>,
        <<"cache-control">> => <<"no-store">>
    },
    cowboy_req:reply(Status, Headers, thoas:encode(json_safe(Body)), Req).

%%====================================================================
%% Internal functions
%%====================================================================

to_binary(Value) when is_binary(Value) ->
    Value;
to_binary(Value) when is_atom(Value) ->
    atom_to_binary(Value, utf8);
to_binary(Value) when is_list(Value) ->
    unicode:characters_to_binary(Value, utf8);
to_binary(Value) ->
    unicode:characters_to_binary(io_lib:format("~p", [Value]), utf8).

json_safe(Value) when is_map(Value) ->
    maps:from_list([{json_safe_key(K), json_safe(V)} || {K, V} <- maps:to_list(Value)]);
json_safe(Value) when is_list(Value) ->
    [json_safe(Item) || Item <- Value];
json_safe(Value) when is_tuple(Value) ->
    json_safe(tuple_to_list(Value));
json_safe(Value) when is_binary(Value) ->
    case unicode:characters_to_binary(Value, utf8, utf8) of
        Value ->
            Value;
        _ ->
            base64:encode(Value)
    end;
json_safe(Value) ->
    Value.

json_safe_key(Key) when is_binary(Key) ->
    case unicode:characters_to_binary(Key, utf8, utf8) of
        Key ->
            Key;
        _ ->
            base64:encode(Key)
    end;
json_safe_key(Key) ->
    Key.
