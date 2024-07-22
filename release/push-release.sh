#!/bin/bash

# 需提前将最新版本号写入根目录 VERSION 内，并执行git add / git commit，此脚本只完成版本号更新、推送git
# 更新版本号 打包release下载包，推送aliyun镜像，推送docker镜像

# 获取最近一次提交的哈希值
LAST_COMMIT_HASH=$(git rev-parse HEAD)

# 检查最近一次提交中是否包含 VERSION 文件
if git diff-tree --no-commit-id --name-only -r $LAST_COMMIT_HASH | grep -q "VERSION"; then
    echo "VERSION file is updated. Updating version..."

    # 这里可以添加更新版本的逻辑
    # 例如，读取 VERSION 文件，增加版本号，然后写回文件
    # 假设 VERSION 文件格式为简单的版本号，如 1.0.0
    VERSION=$(grep -oP '(?<=Version\s).*' VERSION | head -1)
    IFS='.' read -r MAJOR MINOR PATCH <<< "$VERSION"
    ((MINOR++))
    VERSION="${MAJOR}.${MINOR}.0"
    sed -i "s/Version\s\+[0-9]+\.[0-9]+\.[0-9]+/Version ${VERSION}/" VERSION

    # 再次提交 VERSION 文件的变更
    git add VERSION
    git commit -m "Update VERSION to $VERSION"

    # 推送到远程仓库
    git push
else
    echo "No update in VERSION file. Only pushing changes..."
    git push
fi

VERSION=`cat VERSION`

for c in apps/*/rebar.config
do
    sed -i.bck \
    -e "s/zotonic_core,$/{zotonic_core, \"$VERSION\"},/" \
    -e "s/zotonic_notifier,$/{zotonic_notifier, \"$VERSION\"},/" \
    -e "s/zotonic_listen_smtp,$/{zotonic_listen_smtp, \"$VERSION\"},/" \
    -e "s/zotonic_listen_http,$/{zotonic_listen_http, \"$VERSION\"},/" \
    -e "s/zotonic_listen_mqtt,$/{zotonic_listen_mqtt, \"$VERSION\"},/" \
    -e "s/zotonic_filehandler,$/{zotonic_filehandler, \"$VERSION\"},/" \
    -e "s/zotonic_fileindexer,$/{zotonic_fileindexer, \"$VERSION\"},/" \
    -e "s/zotonic_filewatcher,$/{zotonic_filewatcher, \"$VERSION\"},/" \
    -e "s/zotonic_launcher,$/{zotonic_launcher, \"$VERSION\"},/" \
    -e "s/\(zotonic_mod_[a-z_]*\),$/{\1, \"$VERSION\"},/" \
    $c

    sed -i.bck \
    -e "s/zotonic_core$/{zotonic_core, \"$VERSION\"}/" \
    -e "s/zotonic_notifier$/{zotonic_notifier, \"$VERSION\"}/" \
    -e "s/zotonic_listen_smtp$/{zotonic_listen_smtp, \"$VERSION\"}/" \
    -e "s/zotonic_listen_http$/{zotonic_listen_http, \"$VERSION\"}/" \
    -e "s/zotonic_listen_mqtt$/{zotonic_listen_mqtt, \"$VERSION\"}/" \
    -e "s/zotonic_filehandler$/{zotonic_filehandler, \"$VERSION\"}/" \
    -e "s/zotonic_fileindexer$/{zotonic_fileindexer, \"$VERSION\"}/" \
    -e "s/zotonic_filewatcher$/{zotonic_filewatcher, \"$VERSION\"}/" \
    -e "s/zotonic_launcher$/{zotonic_launcher, \"$VERSION\"}/" \
    -e "s/\(zotonic_mod_[a-z_]*\)$/{\1, \"$VERSION\"}/" \
    $c

    sed -i.bck \
    -e "s/zotonic_core, *\".*\"/zotonic_core, \"$VERSION\"/" \
    -e "s/zotonic_notifier, *\".*\"/zotonic_notifier, \"$VERSION\"/" \
    -e "s/zotonic_listen_smtp, *\".*\"/zotonic_listen_smtp, \"$VERSION\"/" \
    -e "s/zotonic_listen_http, *\".*\"/zotonic_listen_http, \"$VERSION\"/" \
    -e "s/zotonic_listen_mqtt, *\".*\"/zotonic_listen_mqtt, \"$VERSION\"/" \
    -e "s/zotonic_filehandler, *\".*\"/zotonic_filehandler, \"$VERSION\"/" \
    -e "s/zotonic_fileindexer, *\".*\"/zotonic_fileindexer, \"$VERSION\"/" \
    -e "s/zotonic_filewatcher, *\".*\"/zotonic_filewatcher, \"$VERSION\"/" \
    -e "s/zotonic_launcher, *\".*\"/zotonic_launcher, \"$VERSION\"/" \
    -e "s/\(zotonic_mod_[a-z_]*\), *\".*\"/\1, \"$VERSION\"/" \
    $c
done

# Delete old artifcats
rm -rf _build
rm -f rebar.lock

# First publish core dependencies for other apps

rebar3 compile

rebar3 hex publish -r hexpm --yes

# Wait for Hex to update its index
sleep 20
rebar3 update

# Cleanup
# rm -rf _build
# rm -f rebar.lock
