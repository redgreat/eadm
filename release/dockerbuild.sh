#!/usr/bin/env bash
set -euo pipefail

MODE=${1:-local}
ROOT_DIR="$(cd "$(dirname "$0")/.." && pwd)"

bump_patch() {
  local ver="$1"
  IFS='.' read -r a b c <<< "$ver"
  c=$((c + 1))
  echo "${a}.${b}.${c}"
}

if [ "$MODE" = "release" ]; then
  current_version="$(cat "$ROOT_DIR/VERSION")"
  new_version="$(bump_patch "$current_version")"
  echo "$new_version" > "$ROOT_DIR/VERSION"
  sed -i "s/{release, {eadm, \"[0-9.]*\"}/{release, {eadm, \"${new_version}\"}/" "$ROOT_DIR/rebar.config"
  sed -i "s/{vsn, \"[0-9.]*\"}/{vsn, \"${new_version}\"}/" "$ROOT_DIR/src/eadm.app.src"
  docker build \
    --build-arg DOCKER_IMAGE_VERSION="$new_version" \
    --build-arg BUILD_PROFILE=prod \
    -t redgreat/eadm:"$new_version" \
    -t redgreat/eadm:latest \
    "$ROOT_DIR"
else
  docker build \
    --build-arg DOCKER_IMAGE_VERSION=local \
    --build-arg BUILD_PROFILE=prod \
    -t eadm:local \
    "$ROOT_DIR"
  docker stop eadm >/dev/null 2>&1 || true
  docker rm eadm >/dev/null 2>&1 || true
  docker run -itd --name eadm -p 8080:8090 eadm:local
  docker logs -n 100 eadm
fi
