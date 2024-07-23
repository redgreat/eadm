# 获取最近的提交哈希值
$RemoteRepoUrl = "https://github.com/redgreat/eadm"
$LastCommitHash = git rev-parse HEAD

# 检查最近的提交中是否包含 VERSION 文件
$IsVersionUpdated = git diff-tree --no-commit-id --name-only -r $LastCommitHash | Select-String "VERSION"

if ($null -ne $IsVersionUpdated) {
    # 获取远程仓库的VERSION文件内容
    $remoteVersionContent = (Invoke-WebRequest -Uri "$RemoteRepoUrl/raw/main/VERSION" | Select-Object -ExpandProperty Content) -replace '\r?\n', ''

    # 获取最后一次提交的 VERSION 内容
    $lastCommitVersionContent = git show HEAD:VERSION

    # 判断提交的VERSION 与 远程 VERSION 是否一致
    if ($lastCommitVersionContent -ne $remoteVersionContent) {
        if ($lastCommitVersionContent -gt $remoteVersionContent) {
            # 更新代码文件内版本号
            # rebar.conf
            $content = Get-Content ../rebar.conf -Raw
            $newContent = [regex]::Replace($content, '(?<=\{release, \{eadm, ")\d+\.\d+\.\d+(?="\}\})', $lastCommitVersionContent.Trim())
            $finalContent = [regex]::Replace($newContent, '(?<=releases/)(\d+\.\d+\.\d+)(?=/prod_db.config)', $lastCommitVersionContent.Trim())
            $finalContent | Set-Content ../rebar.conf

            # app.src
            $content = Get-Content ..\src\eadm.app.src -Raw
            $newContent = [regex]::Replace($content, '(?<=\{vsn, ")(\d+\.\d+\.\d+)(?="\},)', $lastCommitVersionContent.Trim())
            $newContent | Set-Content ../src/eadm.app.src

            # docker-compose.yml
            $content = Get-Content ../docker-compose.yml -Raw
            $newContent = [regex]::Replace($content, '(?<=releases/)(\d+\.\d+\.\d+)(?=/)', $lastCommitVersionContent.Trim())
            $newContent | Set-Content ../docker-compose.yml

            # 添加标签
            $newVersionTag = "v" + $lastCommitVersionContent.Trim()
            git -c credential.helper= -c core.quotepath=false -c log.showSignature=false tag $newVersionTag $LastCommitHash

            # Push 代码
            git -c credential.helper= -c core.quotepath=false -c log.showSignature=false push --progress --porcelain origin refs/heads/main:main --tags
        } else {
            Write-Host "本地版本号小于远程版本号，无法进行版本更新..." -ForegroundColor Red
        }
    } else {
        Write-Host "本地版本号与远程版本一致，版本未更新..." -ForegroundColor Red
    }
} else {
    Write-Host "未提交版本更新..." -ForegroundColor Red
}
