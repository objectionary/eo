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
    File f = path.split('/').inject(basedir.toPath().resolve('target').toFile()) { File dir, String step ->
        File numbered = dir.listFiles()?.find { File sub ->
            step.startsWith('NN-') && sub.directory &&
                sub.name.matches('\\d{2,}-' + java.util.regex.Pattern.quote(step.substring(3)))
        }
        numbered ?: new File(dir, step)
    }
    if (!f.exists()) {
        fail("The file '${f}' is not present")
    }
    log.info("The file is found: ${f}")
}
