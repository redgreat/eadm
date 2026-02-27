%%%-------------------------------------------------------------------
%%% @author eadm
%%% @copyright (C) 2026, REDGREAT
%%% @doc
%%% 安全响应头插件
%%% 为所有HTTP响应添加安全相关的头部，满足生产环境安全要求
%%% 包括HSTS、X-Content-Type-Options、X-Frame-Options等
%%% @end
%%% Created : 2026-02-27 10:00:00
%%%-------------------------------------------------------------------
-module(eadm_security_headers_plugin).
-author("eadm").

%%%===================================================================
%%% Behaviour
%%%===================================================================
-behaviour(nova_plugin).

%%%===================================================================
%%% 函数导出
%%%===================================================================
-export([
    pre_request/2,
    post_request/2,
    plugin_info/0
]).

%%====================================================================
%% Nova Plugin回调函数
%%====================================================================

%% @doc
%% 插件信息
%% @end
plugin_info() ->
    #{
        name => <<"eadm_security_headers_plugin">>,
        version => <<"1.0.0">>,
        description => <<"Adds security headers to HTTP responses">>
    }.

%% @doc
%% 请求前处理（不做任何操作）
%% @end
pre_request(Req, State) ->
    {ok, Req, State}.

%% @doc
%% 响应后处理 - 添加安全响应头
%% @end
post_request(Req, State) ->
    %% 定义安全响应头
    SecurityHeaders = #{
        %% HSTS - 强制使用HTTPS
        %% max-age=31536000: 1年有效期
        %% includeSubDomains: 包含所有子域名
        %% preload: 允许加入HSTS预加载列表
        <<"strict-transport-security">> => 
            <<"max-age=31536000; includeSubDomains; preload">>,
        
        %% 防止MIME类型嗅探
        %% 浏览器必须遵守Content-Type声明的类型
        <<"x-content-type-options">> => <<"nosniff">>,
        
        %% XSS保护
        %% 启用浏览器内置的XSS过滤器
        <<"x-xss-protection">> => <<"1; mode=block">>,
        
        %% 防止点击劫持
        %% DENY: 不允许在任何frame中显示
        <<"x-frame-options">> => <<"DENY">>,
        
        %% 内容安全策略
        %% default-src 'self': 默认只允许同源内容
        %% 注意：根据实际需求可能需要调整此策略
        <<"content-security-policy">> => 
            <<"default-src 'self'; script-src 'self' 'unsafe-inline' 'unsafe-eval'; style-src 'self' 'unsafe-inline'; img-src 'self' data: https:; font-src 'self' data:">>,
        
        %% Referrer策略
        %% strict-origin-when-cross-origin: 跨域时只发送源信息
        <<"referrer-policy">> => <<"strict-origin-when-cross-origin">>,
        
        %% 权限策略（可选）
        %% 限制浏览器功能的使用
        <<"permissions-policy">> => 
            <<"geolocation=(), microphone=(), camera=()">>
    },
    
    %% 获取现有响应头
    ExistingHeaders = maps:get(headers, Req, #{}),
    
    %% 合并安全头部（安全头部优先）
    UpdatedHeaders = maps:merge(ExistingHeaders, SecurityHeaders),
    
    %% 更新请求对象
    UpdatedReq = Req#{headers => UpdatedHeaders},
    
    {ok, UpdatedReq, State}.
