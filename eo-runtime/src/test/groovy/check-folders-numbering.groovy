/**
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

import java.nio.file.Path
import java.util.stream.Collectors

Path target = basedir.toPath().resolve('target/eo')
List<File> directories = target.toFile().listFiles((FileFilter) { File file -> file.directory })
List<String> allowed = [
    '1-parse',
    '2-shake',
    '4-pull',
    '5-lint',
    '6-resolve',
    '7-pre',
    '8-transpile',
    'phi',
    'unphi',
]
List<File> allowedDirs = allowed.stream()
    .map { dirName -> target.resolve(dirName).toFile() }
    .collect(Collectors.toList())

for (dir in directories) {
    if (!allowedDirs.contains(dir)) {
        fail("The directory '${dir.name}' is not expected to be here")
    }
    log.info("The directory is found: ${dir}")
}
