
%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2025, REDGREAT
%% @doc
%%
%% 根据生成加密密码
%%
%% @end
%%% Created : 2025-4-30 13:53:36
%%%-------------------------------------------------------------------
-module(eadm_sec).
-author("wangcw").

%%%===================================================================
%%% 函数导出
%%%===================================================================
-export([generate_sec/1]).

%%====================================================================
%% API 函数
%%====================================================================

%% @doc
%% 生成加密密码
%% base64:encode(crypto:hash(sha256, <<"aVkmcGh2qbLqtpY8NXqcUA==", "Mm19890425">>)).
%% @end
generate_sec(SecString) ->
    SecretKey = application:get_env(eadm, secret_key, <<>>),
    io:format("SecretKey: ~p~n", [SecretKey]),
    base64:encode(crypto:hash(sha256, <<SecretKey/binary, SecString>>)).
