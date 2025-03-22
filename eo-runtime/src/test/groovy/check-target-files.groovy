/**
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
List<String> expected = [
  'eo-foreign.csv',
  'eo/1-parse/org/eolang/bytes.xmir',
  'eo/1-parse/org/eolang/fs/dir.xmir',
  'eo/2-shake/org/eolang/error.xmir',
  'eo/2-shake/org/eolang/sys/os.xmir',
  'eo/5-lint/org/eolang/go.xmir',
  'eo/8-transpile/org/eolang/malloc.xmir',
  'eo/phi/org/eolang/number.phi',
  'eo-test/1-parse/org/eolang/bool-tests.xmir',
  'eo-test/2-shake/org/eolang/go-tests.xmir',
  'eo-test/5-lint/org/eolang/dataized-tests.xmir',
  'eo-test/8-transpile/org/eolang/runtime-tests.xmir',
  'eo-test/phi/org/eolang/number-tests.phi',
  'generated-sources/EOorg/EOeolang/EOdataized.java',
  'generated-sources/EOorg/EOeolang/EOnet/EOsocket.java',
  'generated-test-sources/EOorg/EOeolang/EOtests_and_with_zeroTest.java',
  'classes/EOorg/package-info.class',
  'classes/EOorg/EOeolang/package-info.class',
  'classes/EOorg/EOeolang/EOsys/package-info.class',
  'classes/EOorg/EOeolang/EOmath/package-info.class',
  'classes/EOorg/EOeolang/EOfs/package-info.class',
  'classes/EOorg/EOeolang/EOtxt/package-info.class',
]

for (path in expected) {
    File f = basedir.toPath().resolve('target').resolve(path).toFile()
    if (!f.exists()) {
        fail("The file '${f}' is not present")
    }
    log.info("The file is found: ${f}")
}
