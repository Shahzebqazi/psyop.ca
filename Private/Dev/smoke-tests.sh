#!/usr/bin/env bash
set -euo pipefail

HOST=${HOST:-http://localhost:8080}

pass() { echo "[PASS] $1"; }
fail() { echo "[FAIL] $1"; exit 1; }

echo "Running smoke tests against ${HOST}"

code=$(curl -s -o /dev/null -w "%{http_code}" "${HOST}/")
[[ "$code" == "200" ]] && pass "/ returns 200" || fail "/ returns ${code}"

for path in \
  /css/style.css \
  /css/components.css \
  /css/responsive.css \
  /assets/graphics/white/promo_1.jpg \
  /assets/album-covers/single_spotify_soundcloud_bandcamp.jpg
do
  code=$(curl -s -o /dev/null -w "%{http_code}" "${HOST}${path}")
  [[ "$code" == "200" ]] && pass "${path} returns 200" || fail "${path} returns ${code}"
done

echo "All smoke tests passed."

