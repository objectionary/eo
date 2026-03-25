/**
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
List<String> expected = [
  'eo-foreign.csv',
  'eo/1-parse/bytes.xmir',
  'eo/1-parse/fs/dir.xmir',
  'eo/5-transpile/malloc.xmir',
  'eo/5-transpile/runtime.xmir',
  'generated-sources/org/eolang/EOdataized.java',
  'generated-sources/org/eolang/EOnk/EOsocket.java',
  'generated-test-sources/org/eolang/EOtryEOAtomTest.java',
  'classes/org/eolang/package-info.class',
  'classes/org/eolang/EOsm/package-info.class',
  'classes/org/eolang/EOms/package-info.class',
  'classes/org/eolang/EOfs/package-info.class',
  'classes/org/eolang/EOtt/package-info.class',
]

for (path in expected) {
    File f = basedir.toPath().resolve('target').resolve(path).toFile()
    if (!f.exists()) {
        fail("The file '${f}' is not present")
    }
    log.info("The file is found: ${f}")
}
