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
Path folder = basedir.toPath().resolve('src').resolve('test').resolve('groovy')
List<String> tests = [
  'check-parameters-names.groovy'
]
for (test in tests) {
    Object res = evaluate folder.resolve(test).toFile()
    log.info("Verified with ${test} - OK. Result: ${res}")
}
true
