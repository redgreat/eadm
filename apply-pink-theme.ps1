# 将 Vendor CSS 改为淡红色主题
Write-Host "开始处理..." -ForegroundColor Magenta

$colorMap = @{
    '#0d6efd' = '#ff6b9d'
    '#0a58ca' = '#e55a8a'
    '#084298' = '#cc4870'
    '13, 110, 253' = '255, 107, 157'
    '#198754' = '#ff8fa3'
    '#146c43' = '#e67a8f'
    '#0f5132' = '#cc6579'
    '25, 135, 84' = '255, 143, 163'
    '#0dcaf0' = '#ffb3c6'
    '#31d2f2' = '#ffc2d4'
    '#087990' = '#cc8fa3'
    '13, 202, 240' = '255, 179, 198'
    '#428bca' = '#ff6b9d'
    '#92bce0' = '#ffb3c6'
    '#b9d4ec' = '#ffd4e0'
}

$allFiles = Get-ChildItem -Path "priv/assets/vendor" -Filter "*.css" -Recurse -File
Write-Host "找到 $($allFiles.Count) 个文件"

$modified = 0
$total = 0

foreach ($fileObj in $allFiles) {
    $file = $fileObj.FullName
    $content = Get-Content $file -Raw -Encoding UTF8
    $original = $content
    
    foreach ($old in $colorMap.Keys) {
        $new = $colorMap[$old]
        $pattern = "(?i)" + [regex]::Escape($old)
        if ($content -match $pattern) {
            $count = ([regex]::Matches($content, $pattern)).Count
            $content = $content -replace $pattern, $new
            $total += $count
        }
    }
    
    if ($content -ne $original) {
        Set-Content $file $content -NoNewline -Encoding UTF8
        $modified++
    }
}

Write-Host "修改了 $modified 个文件，共 $total 处" -ForegroundColor Green
