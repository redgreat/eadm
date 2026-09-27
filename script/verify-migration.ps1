param(
    [switch]$SkipFrontend
)

$ErrorActionPreference = "Stop"

$Root = Resolve-Path (Join-Path $PSScriptRoot "..")
$Tmp = Join-Path $Root ".tmp_compile"

function Step($Message) {
    Write-Host ""
    Write-Host "==> $Message" -ForegroundColor Cyan
}

try {
    Step "Prepare Erlang compile output"
    if (Test-Path $Tmp) {
        Remove-Item -LiteralPath $Tmp -Recurse -Force
    }
    New-Item -ItemType Directory -Force $Tmp | Out-Null

    Step "Compile Cowboy Erlang modules"
    & erlc -o $Tmp `
        (Join-Path $Root "src\eadm_cowboy_http.erl") `
        (Join-Path $Root "src\eadm_cowboy_req.erl") `
        (Join-Path $Root "src\eadm_cowboy_session.erl") `
        (Join-Path $Root "src\eadm_cowboy_guard.erl") `
        (Join-Path $Root "src\eadm_cowboy_ping_handler.erl") `
        (Join-Path $Root "src\eadm_cowboy_system_handler.erl") `
        (Join-Path $Root "src\eadm_cowboy_users_handler.erl") `
        (Join-Path $Root "src\eadm_cowboy_roles_handler.erl") `
        (Join-Path $Root "src\eadm_cowboy_devices_handler.erl") `
        (Join-Path $Root "src\eadm_cowboy_crontabs_handler.erl") `
        (Join-Path $Root "src\eadm_cowboy_health_handler.erl") `
        (Join-Path $Root "src\eadm_cowboy_location_handler.erl") `
        (Join-Path $Root "src\eadm_cowboy_finance_handler.erl") `
        (Join-Path $Root "src\eadm_cowboy_auth_handler.erl") `
        (Join-Path $Root "src\eadm_auth_service.erl") `
        (Join-Path $Root "src\eadm_dashboard_service.erl") `
        (Join-Path $Root "src\eadm_system_service.erl") `
        (Join-Path $Root "src\eadm_user_service.erl") `
        (Join-Path $Root "src\eadm_role_service.erl") `
        (Join-Path $Root "src\eadm_device_service.erl") `
        (Join-Path $Root "src\eadm_health_service.erl") `
        (Join-Path $Root "src\eadm_location_service.erl") `
        (Join-Path $Root "src\eadm_finance_service.erl") `
        (Join-Path $Root "src\eadm_crontab_service.erl") `
        (Join-Path $Root "src\eadm_api_response.erl") `
        (Join-Path $Root "src\eadm_spa_handler.erl")

    if ($LASTEXITCODE -ne 0) {
        throw "erlc failed with exit code $LASTEXITCODE"
    }

    Step "Assert API response helpers"
    $Eval = @"
Ok = eadm_api_response:ok(#{<<"value">> => 1}, <<"done">>),
true = maps:get(<<"success">>, Ok),
<<"ok">> = maps:get(<<"code">>, Ok),
<<"done">> = maps:get(<<"message">>, Ok),
#{<<"value">> := 1} = maps:get(<<"data">>, Ok),
Err = eadm_api_response:validation_error(<<"bad">>),
false = maps:get(<<"success">>, Err),
<<"validation_error">> = maps:get(<<"code">>, Err),
SystemItems = eadm_system_service:info(),
true = is_list(SystemItems),
true = lists:any(fun(#{<<"key">> := <<"otpRelease">>}) -> true; (_) -> false end, SystemItems),
application:set_env(eadm, secret_key, <<"test-secret">>),
Token = eadm_cowboy_session:sign(#{<<"loginName">> => <<"admin">>}),
{ok, #{<<"loginName">> := <<"admin">>}} = eadm_cowboy_session:verify(Token),
{error, invalid_signature} = eadm_cowboy_session:verify(<<Token/binary, <<"x">>/binary>>),
{module, eadm_cowboy_guard} = code:ensure_loaded(eadm_cowboy_guard),
true = erlang:function_exported(eadm_cowboy_guard, require, 2),
halt(0).
"@
    & erl -pa $Tmp -noshell -eval $Eval

    if ($LASTEXITCODE -ne 0) {
        throw "Erlang assertions failed with exit code $LASTEXITCODE"
    }

    if (-not $SkipFrontend) {
        Step "Build SolidJS frontend"
        Push-Location (Join-Path $Root "frontend")
        try {
            & npm run build
            if ($LASTEXITCODE -ne 0) {
                throw "npm run build failed with exit code $LASTEXITCODE"
            }
            $IndexHtml = Join-Path (Get-Location) "dist\index.html"
            $IndexContent = Get-Content $IndexHtml -Raw
            if ($IndexContent -notmatch "/assets/") {
                throw "frontend dist index.html does not reference /assets/"
            }
        }
        finally {
            Pop-Location
        }
    }

    Step "Cowboy/SolidJS verification passed"
}
finally {
    if (Test-Path $Tmp) {
        Remove-Item -LiteralPath $Tmp -Recurse -Force
    }
}
