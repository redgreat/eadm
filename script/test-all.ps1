param(
    [switch]$RunDialyzer,
    [switch]$SkipAudit
)

$ErrorActionPreference = "Stop"
$Root = Resolve-Path (Join-Path $PSScriptRoot "..")
$Rebar = Get-Command rebar3 -ErrorAction SilentlyContinue
if ($null -eq $Rebar) {
    $RebarCommand = Join-Path $Root "tools\rebar3.cmd"
} else {
    $RebarCommand = $Rebar.Source
}

function Invoke-Step($Name, [scriptblock]$Action) {
    Write-Host ""
    Write-Host "==> $Name" -ForegroundColor Cyan
    & $Action
    if ($LASTEXITCODE -ne 0) {
        throw "$Name failed with exit code $LASTEXITCODE"
    }
}

Push-Location $Root
try {
    Invoke-Step "Erlang compile with warnings as errors" { & $RebarCommand as test compile }
    Invoke-Step "Erlang EUnit suite with coverage" { & $RebarCommand as test eunit }
    Invoke-Step "Erlang xref" { & $RebarCommand xref }
    if ($RunDialyzer) {
        Invoke-Step "Erlang Dialyzer" { & $RebarCommand dialyzer }
    }
    Invoke-Step "Cowboy/SolidJS migration checks" { & (Join-Path $Root "script\verify-migration.ps1") -SkipFrontend }

    Push-Location (Join-Path $Root "frontend")
    try {
        Invoke-Step "Frontend lint" { npm run lint }
        Invoke-Step "Frontend unit tests and coverage" { npm run test:coverage }
        Invoke-Step "Frontend production build" { npm run build }
        Invoke-Step "Frontend browser E2E" { npm run test:e2e }
        if (-not $SkipAudit) {
            Invoke-Step "Frontend production dependency audit" { npm audit --omit=dev --audit-level=high }
        }
    } finally {
        Pop-Location
    }

    Write-Host ""
    Write-Host "All automated checks passed." -ForegroundColor Green
} finally {
    Pop-Location
}
