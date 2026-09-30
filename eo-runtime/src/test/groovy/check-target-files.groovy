/**
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
/**
 * A step "NN-name" stands for the directory "name" with whatever number
 * the build gave it, since the numbers depend on the order of the goals.
 */
List<String> expected = [
  'eo-foreign.csv',
  'eo/NN-parse/bytes.xmir',
  'eo/NN-parse/directory.xmir',
  'eo/NN-transpile/malloc.xmir',
  'generated-sources/org/eolang/EOseq.java',
  'generated-sources/org/eolang/EOsocket.java',
  'generated-test-sources/org/eolang/TestEObytes.java',
  'classes/org/eolang/package-info.class',
]

for (path in expected) {
    File f = basedir.toPath().resolve('target').toFile()
    for (step in path.split('/')) {
        if (step.startsWith('NN-')) {
            String name = step.substring(3)
            File numbered = f.listFiles()?.find {
                it.directory && it.name.matches('\\d{2,}-' + java.util.regex.Pattern.quote(name))
            }
            f = numbered ?: new File(f, step)
        } else {
            f = new File(f, step)
        }
    }
    if (!f.exists()) {
        fail("The file '${f}' is not present")
    }
    log.info("The file is found: ${f}")
}
