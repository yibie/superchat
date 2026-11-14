#!/bin/sh
# 下载上游 @earendil-works/pi-ai 的模型目录数据（dist/providers/data/）到 data/，
# 并按上游 .manifest.json 里的 shasum 逐个校验 sha256。
#
# 用法：sh sync/fetch-model-data.sh
# 环境：PI_PKG_VERSION（默认 1.4.0）、PI_DATA_BASE（默认 unpkg 上该版本的数据目录）
#
# 退出码：0 全部下载且校验通过；1 下载失败或校验不符；2 环境缺少 curl/shasum。
set -eu
VERSION="${PI_PKG_VERSION:-1.4.0}"
BASE="${PI_DATA_BASE:-https://unpkg.com/@earendil-works/pi-ai@${VERSION}/dist/providers/data}"
PKG="@earendil-works/pi-ai@${VERSION}"

ROOT=$(cd "$(dirname "$0")/.." && pwd)
OUT="$ROOT/data"
TMP="$OUT/.tmp.$$"
MANIFEST_NAME=".manifest.json"

command -v curl >/dev/null 2>&1 || { echo "缺少 curl" >&2; exit 2; }
command -v shasum >/dev/null 2>&1 || { echo "缺少 shasum" >&2; exit 2; }

cleanup() { rm -rf "$TMP"; }
trap cleanup EXIT INT TERM

rm -rf "$TMP"
mkdir -p "$TMP"
mkdir -p "$OUT"

# fetch <相对路径> <目标文件>：失败重试一次
fetch() {
  url="$BASE/$1"
  dest="$2"
  if ! curl -fsSL --connect-timeout 15 --max-time 120 --retry 1 -o "$dest" "$url"; then
    echo "下载失败：$url" >&2
    exit 1
  fi
}

echo "来源：$PKG  (commit 见 README 同步基线)"
echo "数据目录：$BASE"

fetch "$MANIFEST_NAME" "$TMP/$MANIFEST_NAME"
msize=$(wc -c <"$TMP/$MANIFEST_NAME" | tr -d ' ')
echo "已取 .manifest.json：$msize 字节"

# manifest 形如 {"files": {"anthropic.json": "<sha256>", ...}}
# 用 grep/sed 逐个取出「文件名 空格 哈希」对，不依赖 jq 或 emacs。
entries="$TMP/entries"
tr -d '\n\r' <"$TMP/$MANIFEST_NAME" \
  | grep -oE '"[^"]+\.json"[[:space:]]*:[[:space:]]*"[0-9a-f]{64}"' \
  | sed -e 's/^"//' -e 's/"[[:space:]]*:[[:space:]]*"/ /' -e 's/"$//' \
  >"$entries"

count=$(wc -l <"$entries" | tr -d ' ')
if [ "$count" -eq 0 ]; then
  echo "manifest 里没有解析出文件条目，格式可能变了" >&2
  exit 1
fi
echo "manifest 列出 $count 个文件"

ok=0
bad=0
while read -r name want; do
  [ -n "$name" ] || continue
  fetch "$name" "$TMP/$name"
  got=$(shasum -a 256 "$TMP/$name" | awk '{print $1}')
  if [ "$got" = "$want" ]; then
    ok=$((ok + 1))
  else
    bad=$((bad + 1))
    echo "校验不符：$name  期望 $want  实际 $got" >&2
  fi
done <"$entries"

if [ "$bad" -ne 0 ]; then
  echo "失败：$bad/$count 个文件 sha256 不符，data/ 未更新" >&2
  exit 1
fi

# 全部校验通过才落盘：先清掉旧的本地副本，再把临时目录里的内容移入 data/。
for f in "$OUT"/*.json "$OUT/.manifest.json"; do
  [ -e "$f" ] && rm -f "$f"
done
mv "$TMP/$MANIFEST_NAME" "$OUT/$MANIFEST_NAME"
mv "$TMP"/*.json "$OUT"/
rm -f "$entries"

echo "完成：$ok/$count 个文件 sha256 校验通过，已写入 $OUT"
