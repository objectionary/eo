/**
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
List<String> expected = [
  'eo-foreign.csv',
  'eo/1-parse/bytes.xmir',
  'eo/1-parse/directory.xmir',
  'eo/5-transpile/malloc.xmir',
  'generated-sources/org/eolang/EOseq.java',
  'generated-sources/org/eolang/EOsocket.java',
  'generated-test-sources/org/eolang/EObytesTest.java',
  'classes/org/eolang/package-info.class',
  'classes/org/eolang/EO_number/package-info.class',
  'classes/org/eolang/EO_string/package-info.class',
]

for (path in expected) {
    File f = basedir.toPath().resolve('target').resolve(path).toFile()
    if (!f.exists()) {
        fail("The file '${f}' is not present")
    }
    log.info("The file is found: ${f}")
}
