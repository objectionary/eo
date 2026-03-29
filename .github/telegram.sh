#!/usr/bin/env bash
# SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
# SPDX-License-Identifier: MIT

set -euo pipefail

ref_name=$1

git fetch --tags --force
(
  printf 'Good news, we have just published the %d-th release ' \
    "$(git tag | wc -l | xargs)"
  printf 'of [EO-to-Java compiler](https://github.com/objectionary/eo): '
  printf '[%s](https://github.com/objectionary/eo/releases/tag/%s). ' \
    "${ref_name}" "${ref_name}"
  printf 'At the moment, there are %dK lines-of code ' \
    "$(echo "$(jq '.SUM.code' <cloc.json)" / 1000 | bc)"
  printf 'and %dK [hits-of-code](https://www.yegor256.com/2014/11/14/hits-of-code.html) ' \
    "$(echo "$(hoc .)" / 1000 | bc)"
  printf 'in the [repository](https://github.com/objectionary/eo) on GitHub '
  printf '(%d commits total); ' \
    "$(git rev-list --count origin/master)"
  printf '[%d objects](https://github.com/objectionary/eo/tree/master/eo-runtime/src/main/eo/org/eolang) ' \
    "$(find eo-runtime/src/main/eo -name '*.eo' | wc -l)"
  printf 'and [%d atoms](https://github.com/objectionary/eo/tree/master/eo-runtime/src/main/java/EOorg/EOeolang) ' \
    "$(find eo-runtime/src/main/java/EOorg -name 'EO*.java' | wc -l)"
  printf 'in the [eo-runtime](https://github.com/objectionary/eo/tree/master/eo-runtime) module; '
  printf '[%d issues](https://github.com/objectionary/eo/issues?q=is%%3Aopen+is%%3Aissue+label%%3A%%22help+wanted%%22) ' \
    "$(curl --silent 'https://api.github.com/repos/objectionary/eo/issues?state=open&per_page=100&labels=help%20wanted' | jq length)"
  printf 'in the backlog, where your help is needed!\n\n'
  printf 'You may also be interested in helping us in these repositories:\n\n'
  for r in jeo-maven-plugin phino lints eoc; do
    printf '  - [%s](https://github.com/objectionary/%s) ' "${r}" "${r}"
    printf '([%d issues]' \
      "$(curl --silent "https://api.github.com/repos/objectionary/${r}/issues?state=open&per_page=100&labels=help%20wanted" | jq length)"
    printf '(https://github.com/objectionary/%s/issues?q=is%%3Aopen+is%%3Aissue+label%%3A%%22help+wanted%%22))\n' \
      "${r}"
  done
  printf '\nDo not forget to give us your GitHub stars, this is what we work for!'
) > message.md
