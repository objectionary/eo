#!/usr/bin/env bash
# SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
# SPDX-License-Identifier: MIT

set -euo pipefail

convert_path=$1

mkdir -p "${convert_path}"
wget --quiet http://public.yegor256.com/convert.zip -O /tmp/convert.zip
unzip -o -d "${convert_path}" /tmp/convert.zip

mvn com.yegor256:antlr2ebnf-maven-plugin:0.0.7:generate \
  -pl :eo-parser --errors --batch-mode --quiet \
  "-Dantlr2ebnf.convertDir=${convert_path}" \
  "-Dantlr2ebnf.specials=eof,eol,eop,tab,untab" \
  "-Dantlr2ebnf.margin=40"

git fetch --no-tags origin gh-pages
git checkout gh-pages
mkdir -p ebnf
convert --version
convert --help | grep Delegates
pdfcrop --version
set -x
p="Eo"
cp "eo-parser/target/ebnf/org/eolang/parser/${p}.pdf" .
pdfcrop --margins '10 10 10 10' "${p}.pdf" "${p}-cropped.pdf"
pdf2svg "${p}-cropped.pdf" "${p}.svg"
convert -verbose -density 300 -quality 100 -colorspace RGB "${p}.svg" "${p}.png"
cp "${p}.png" ebnf/
cp "${p}.svg" ebnf/
