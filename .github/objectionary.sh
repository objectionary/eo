#!/usr/bin/env bash
# SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
# SPDX-License-Identifier: MIT

set -euo pipefail

tag=$1
mkdir -p gh-pages/xsd
mkdir -p gh-pages/objectionary
cp eo-parser/target/classes/XMIR.xsd gh-pages/
cp gh-pages/XMIR.xsd "gh-pages/xsd/XMIR-${tag}.xsd"
cp -R eo-runtime/src/main/eo/* gh-pages/objectionary/
find gh-pages/objectionary -name '*.eo' \
  -exec sed -i "s/jvm org.eolang:eo-runtime:0\.0\.0/jvm org.eolang:eo-runtime:${tag}/g" {} +
find gh-pages/objectionary -name '*.eo' \
  -exec sed -i "s/version 0\.0\.0/version ${tag}/g" {} +
find gh-pages/objectionary -name '*.eo' -printf 'objectionary/%P\n' > gh-pages/objectionary.lst
