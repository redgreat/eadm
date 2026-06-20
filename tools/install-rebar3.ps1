$ErrorActionPreference = "Stop"

$ToolsDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$Rebar3Path = Join-Path $ToolsDir "rebar3"
$Rebar3CmdPath = Join-Path $ToolsDir "rebar3.cmd"

Invoke-WebRequest -Uri "https://s3.amazonaws.com/rebar3/rebar3" -OutFile $Rebar3Path
Set-Content -Path $Rebar3CmdPath -Value "@echo off`r`nescript ""%~dp0rebar3"" %*`r`n" -NoNewline

& $Rebar3CmdPath version
