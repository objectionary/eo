/**
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
List<String> expected = [
  'eo-foreign.csv',
  'eo/1-parse/org/eolang/bytes.xmir',
  'eo/1-parse/org/eolang/fs/dir.xmir',
  'eo/5-transpile/org/eolang/malloc.xmir',
  'eo/phi/org/eolang/number.phi',
  'eo/5-transpile/org/eolang/runtime.xmir',
  'generated-sources/EOorg/EOeolang/EOdataized.java',
  'generated-sources/EOorg/EOeolang/EOnet/EOsocket.java',
  'generated-test-sources/EOorg/EOeolang/EOtryEOAtomTest.java',
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
