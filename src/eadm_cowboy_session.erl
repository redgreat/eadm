%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2024, REDGREAT
%%% @doc
%%%  Lightweight signed-cookie session helper for native Cowboy APIs.
%%% @end
%%%-------------------------------------------------------------------
-module(eadm_cowboy_session).
-author("wangcw").

-export([sign/1, verify/1, set_cookie/2, clear_cookie/1]).

-define(COOKIE_NAME, <<"eadm_session">>).

%%====================================================================
%% API functions
%%====================================================================

sign(Data) ->
    Payload = term_to_binary(Data),
    EncodedPayload = base64url(Payload),
    Signature = signature(EncodedPayload),
    <<EncodedPayload/binary, ".", Signature/binary>>.

verify(Token) when is_binary(Token) ->
    case binary:split(Token, <<".">>) of
        [EncodedPayload, Signature] ->
            case secure_compare(signature(EncodedPayload), Signature) of
                true -> decode_payload(EncodedPayload);
                false -> {error, invalid_signature}
            end;
        _ ->
            {error, invalid_token}
    end;
verify(_) ->
    {error, invalid_token}.

set_cookie(Req, Data) ->
    Token = sign(Data),
    cowboy_req:set_resp_cookie(?COOKIE_NAME, Token, Req, cookie_opts()).

clear_cookie(Req) ->
    Opts = cookie_opts(),
    cowboy_req:set_resp_cookie(?COOKIE_NAME, <<>>, Req, Opts#{max_age => 0}).

%%====================================================================
%% Internal functions
%%====================================================================

decode_payload(EncodedPayload) ->
    try
        Payload = base64url_decode(EncodedPayload),
        {ok, binary_to_term(Payload)}
    catch
        _:_ ->
            {error, invalid_payload}
    end.

signature(EncodedPayload) ->
    Secret = application:get_env(nova, secret_key, <<"">>),
    base64url(crypto:mac(hmac, sha256, Secret, EncodedPayload)).

cookie_opts() ->
    #{
        path => <<"/">>,
        http_only => true,
        same_site => lax
    }.

base64url(Bin) ->
    Trimmed = binary:replace(base64:encode(Bin), <<"=">>, <<>>, [global]),
    Step1 = binary:replace(Trimmed, <<"+">>, <<"-">>, [global]),
    binary:replace(Step1, <<"/">>, <<"_">>, [global]).

base64url_decode(Bin) ->
    Step1 = binary:replace(Bin, <<"-">>, <<"+">>, [global]),
    Step2 = binary:replace(Step1, <<"_">>, <<"/">>, [global]),
    Padding = case byte_size(Step2) rem 4 of
        0 -> <<>>;
        2 -> <<"==">>;
        3 -> <<"=">>;
        _ -> <<>>
    end,
    base64:decode(<<Step2/binary, Padding/binary>>).

secure_compare(A, B) when byte_size(A) =:= byte_size(B) ->
    secure_compare(A, B, 0);
secure_compare(_A, _B) ->
    false.

secure_compare(<<>>, <<>>, Result) ->
    Result =:= 0;
secure_compare(<<A, RestA/binary>>, <<B, RestB/binary>>, Result) ->
    secure_compare(RestA, RestB, Result bor (A bxor B)).
