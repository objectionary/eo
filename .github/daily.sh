#!/usr/bin/env bash
# SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
# SPDX-License-Identifier: MIT

set -euo pipefail

workspace=$1

mvn clean install -Pqulice -PskipTests
script=${workspace}/src/test/scripts/test-repetition.sh
if [ ! -f "${script}" ]; then
  echo "Script ${script} not found"
  exit 1
fi
chmod +x "${script}"
"${script}" --max 10 --folder "${workspace}/eo-parser"
"${script}" --max 10 --folder "${workspace}/eo-maven-plugin"
"${script}" --max 10 --folder "${workspace}/eo-runtime" --compilation true
