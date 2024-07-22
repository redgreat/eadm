#!/bin/bash

# 设置远程仓库URL
REMOTE_REPO_URL="https://github.com/redgreat/eadm"

# 获取最近的提交哈希值
LAST_COMMIT_HASH=$(git rev-parse HEAD)

# 检查最近的提交中是否包含 VERSION 文件
IS_VERSION_UPDATED=$(git diff-tree --no-commit-id --name-only -r $LAST_COMMIT_HASH | grep -q "VERSION"; echo $?)

# 如果提交中包含了 VERSION 文件
if [[ $IS_VERSION_UPDATED -eq 0 ]]; then
# 获取远程仓库的 VERSION 文件内容
REMOTE_VERSION_CONTENT=$(curl -sSL "$REMOTE_REPO_URL/raw/main/VERSION")
REMOTE_VERSION_CONTENT=${REMOTE_VERSION_CONTENT//$'\r'/$'\n'}
REMOTE_VERSION_CONTENT=$(echo "$REMOTE_VERSION_CONTENT" | tr -d '\n')

# 获取最后一次提交的 VERSION 内容
LAST_COMMIT_VERSION_CONTENT=$(git show HEAD:VERSION)

# 判断提交的 VERSION 与远程 VERSION 是否一致
if [[ "$LAST_COMMIT_VERSION_CONTENT" != "$REMOTE_VERSION_CONTENT" ]]; then
if [[ "$LAST_COMMIT_VERSION_CONTENT" > "$REMOTE_VERSION_CONTENT" ]]; then
# 更新代码文件内版本号
# rebar.conf
sed -i "s/\(.*\{release, \{eadm, \"\)\([0-9]\+\.[0-9]\+\.[0-9]\+\)(\".*\}\}\)/\1$LATEST_COMMIT_VERSION_CONTENT\3/" rebar.conf

# app.src
sed -i "s/\(.*\{vsn, \"\)\([0-9]\+\.[0-9]\+\.[0-9]\+\)(\".*\},\)/\1$LATEST_COMMIT_VERSION_CONTENT\3/" src/eadm.app.src

# docker-compose.yml
sed -i "s/\(.*releases\/\)\([0-9]\+\.[0-9]\+\.[0-9]\+\)(\/.*\)/\1$LATEST_COMMIT_VERSION_CONTENT\3/" docker-compose.yml

# 添加标签
NEW_VERSION_TAG="v$LATEST_COMMIT_VERSION_CONTENT"
git tag $NEW_VERSION_TAG $LAST_COMMIT_HASH

# Push 代码
# git push origin $NEW_VERSION_TAG
else
echo "本地版本号小于远程版本号，无法进行版本更新..."
fi
else
echo "本地版本号与远程版本一致，版本未更新..."
fi
else
echo "未提交版本更新..."
fi
