/**
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
List<String> expected = [
  'eo-foreign.csv',
  'eo/1-parse/org/eolang/bytes.xmir',
  'eo/1-parse/org/eolang/fs/dir.xmir',
  'eo/5-transpile/org/eolang/malloc.xmir',
  'eo/5-transpile/org/eolang/runtime.xmir',
  'generated-sources/EOorg/EOeolang/EOdataized.java',
  'generated-sources/EOorg/EOeolang/EOnk/EOsocket.java',
  'generated-test-sources/EOorg/EOeolang/EOtryEOAtomTest.java',
  'classes/EOorg/package-info.class',
  'classes/EOorg/EOeolang/package-info.class',
  'classes/EOorg/EOeolang/EOsm/package-info.class',
  'classes/EOorg/EOeolang/EOms/package-info.class',
  'classes/EOorg/EOeolang/EOfs/package-info.class',
  'classes/EOorg/EOeolang/EOtt/package-info.class',
]

for (path in expected) {
    File f = basedir.toPath().resolve('target').resolve(path).toFile()
    if (!f.exists()) {
        fail("The file '${f}' is not present")
    }
    log.info("The file is found: ${f}")
}
