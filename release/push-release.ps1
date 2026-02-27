param(
    [Parameter(Position=0)]
    [string]$Version = ''
)

$Root = Split-Path -Parent $PSScriptRoot
$VersionFile = Join-Path $Root "VERSION"

function Get-LatestTag {
    $tags = git tag --list 'v*' --sort=-version:refname
    if (-not $tags) { return $null }
    return ($tags | Select-Object -First 1).Trim()
}

function Normalize-Version([string]$v) {
    if ($v.StartsWith('v')) { return $v.Substring(1) }
    return $v
}

function Bump-Patch([string]$v) {
    $m = [regex]::Match($v, '^(\d+)\.(\d+)\.(\d+)(?:-.+)?$')
    if (-not $m.Success) { return $v }
    $a = [int]$m.Groups[1].Value
    $b = [int]$m.Groups[2].Value
    $c = ([int]$m.Groups[3].Value) + 1
    return "$a.$b.$c"
}

if ([string]::IsNullOrWhiteSpace($Version)) {
    $latestTag = Get-LatestTag
    if ($null -eq $latestTag) {
        $Version = "0.0.1"
    }
    else {
        $current = Normalize-Version $latestTag
        $Version = Bump-Patch $current
    }
}

if ($Version.StartsWith('v')) { $Version = $Version.Substring(1) }
$NormalizedVersion = $Version
$VersionForFile = $Version
Set-Content -Path $VersionFile -Value $VersionForFile

$rebarPath = Join-Path $Root "rebar.config"
$rebarContent = Get-Content $rebarPath -Raw
$rebarContent = [regex]::Replace($rebarContent, '(?<=\{release, \{eadm, ")\d+\.\d+\.\d+(?="\}\})', $NormalizedVersion)
$rebarContent | Set-Content $rebarPath

$appSrcPath = Join-Path $Root "src\eadm.app.src"
$appSrcContent = Get-Content $appSrcPath -Raw
$appSrcContent = [regex]::Replace($appSrcContent, '(?<=\{vsn, ")(\d+\.\d+\.\d+)(?="\},)', $NormalizedVersion)
$appSrcContent | Set-Content $appSrcPath

docker build `
  --build-arg DOCKER_IMAGE_VERSION=$NormalizedVersion `
  --build-arg BUILD_PROFILE=prod `
  -t redgreat/eadm:$NormalizedVersion `
  -t redgreat/eadm:latest `
  $Root | Out-Null

git add $VersionFile $rebarPath $appSrcPath | Out-Null
git commit -m "chore: release v$NormalizedVersion" | Out-Null

$tag = "v$NormalizedVersion"
git tag $tag | Out-Null
git push origin $tag | Out-Null
