%%%-------------------------------------------------------------------
%%% @doc
%%% SSL证书管理模块
%%% 提供证书检查、续期和定时任务功能
%%% @end
%%%-------------------------------------------------------------------
-module(eadm_cert_manager).

-behaviour(gen_server).

%% API
-export([start_link/0, check_and_renew/0, get_cert_info/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
    cert_dir :: string(),
    script_path :: string(),
    timer_ref :: reference() | undefined,
    check_interval :: integer()
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc 手动触发证书检查和续期
check_and_renew() ->
    gen_server:call(?SERVER, check_and_renew).

%% @doc 获取证书信息
get_cert_info() ->
    gen_server:call(?SERVER, get_cert_info).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    CertDir = application:get_env(eadm, ssl_cert_dir, "/opt/eadm/certs"),
    ScriptPath = application:get_env(eadm, cert_script, "/opt/eadm/docker/cert-manager.sh"),
    CheckInterval = application:get_env(eadm, cert_check_interval, 86400000),
    
    % 启动时检查一次
    case application:get_env(eadm, ssl_enabled, false) of
        true ->
            logger:info("SSL证书管理模块启动，检查间隔: ~p 毫秒", [CheckInterval]),
            spawn(fun() -> run_cert_script(ScriptPath) end),
            % 设置定时器
            TimerRef = erlang:send_after(CheckInterval, self(), check_cert),
            {ok, #state{
                cert_dir = CertDir, 
                script_path = ScriptPath, 
                timer_ref = TimerRef,
                check_interval = CheckInterval
            }};
        false ->
            logger:info("SSL证书管理模块已禁用"),
            {ok, #state{
                cert_dir = CertDir, 
                script_path = ScriptPath, 
                timer_ref = undefined,
                check_interval = CheckInterval
            }}
    end.

handle_call(check_and_renew, _From, State) ->
    Result = run_cert_script(State#state.script_path),
    {reply, Result, State};

handle_call(get_cert_info, _From, State) ->
    Info = get_certificate_info(State#state.cert_dir),
    {reply, Info, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(check_cert, State) ->
    % 执行证书检查
    logger:info("定时检查SSL证书"),
    spawn(fun() -> run_cert_script(State#state.script_path) end),
    % 重新设置定时器
    TimerRef = erlang:send_after(State#state.check_interval, self(), check_cert),
    {noreply, State#state{timer_ref = TimerRef}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    case State#state.timer_ref of
        undefined -> ok;
        TimerRef -> erlang:cancel_timer(TimerRef)
    end,
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
%% 执行证书管理脚本
run_cert_script(ScriptPath) ->
    case filelib:is_file(ScriptPath) of
        true ->
            % 构建环境变量
            Env = build_env_vars(),
            EnvStr = lists:flatten([io_lib:format("export ~s='~s'; ", [K, V]) || {K, V} <- Env]),
            Command = EnvStr ++ ScriptPath,
            
            logger:info("执行证书管理脚本: ~s", [ScriptPath]),
            case os:cmd(Command) of
                Result ->
                    logger:info("证书检查结果: ~s", [Result]),
                    {ok, Result}
            end;
        false ->
            logger:warning("证书管理脚本不存在: ~s", [ScriptPath]),
            {error, script_not_found}
    end.

%% @private
%% 构建环境变量
build_env_vars() ->
    Domain = application:get_env(eadm, ssl_domain, "example.com"),
    Email = application:get_env(eadm, ssl_email, "admin@example.com"),
    Validation = application:get_env(eadm, ssl_validation, http),
    CertDir = application:get_env(eadm, ssl_cert_dir, "/opt/eadm/certs"),
    DnsProvider = application:get_env(eadm, ssl_dns_provider, ""),
    DnsCredentials = application:get_env(eadm, ssl_dns_credentials, []),
    
    BaseEnv = [
        {"SSL_DOMAIN", Domain},
        {"SSL_EMAIL", Email},
        {"SSL_VALIDATION", atom_to_list(Validation)},
        {"SSL_CERT_DIR", CertDir},
        {"SSL_DNS_PROVIDER", DnsProvider}
    ],
    
    % 添加DNS凭证
    BaseEnv ++ DnsCredentials.

%% @private
%% 获取证书信息
get_certificate_info(CertDir) ->
    CertFile = filename:join(CertDir, "cert.pem"),
    case filelib:is_file(CertFile) of
        true ->
            Command = io_lib:format("openssl x509 -in ~s -noout -subject -issuer -dates", [CertFile]),
            Result = os:cmd(lists:flatten(Command)),
            parse_cert_info(Result);
        false ->
            {error, cert_not_found}
    end.

%% @private
%% 解析证书信息
parse_cert_info(Output) ->
    Lines = string:tokens(Output, "\n"),
    lists:foldl(fun(Line, Acc) ->
        case string:split(Line, "=", leading) of
            [Key, Value] ->
                maps:put(string:trim(Key), string:trim(Value), Acc);
            _ ->
                Acc
        end
    end, #{}, Lines).
