    import java.nio.file.Path

/**
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

/**
 * Entry point for running validation scripts.
 * To add new validation create new script in this folder and add it
 * to the list below.
 */
Path folder = basedir.toPath().resolve('src/test/groovy')
List<String> tests = [
    'check-folders-numbering.groovy',
    'check-all-java-classes-compiled.groovy',
    'check-runtime-deps.groovy',
    'check-target-files.groovy',
]
for (it in tests) {
    evaluate folder.resolve(it).toFile()
}
