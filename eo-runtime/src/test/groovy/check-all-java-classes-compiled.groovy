/**
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

import java.nio.file.Files
import java.nio.file.Path
import java.util.stream.Collectors

Path binaries = basedir.toPath()
    .resolve('target')
    .resolve('classes')
    .resolve('org')
    .resolve('eolang')
Path classes = basedir.toPath()
    .resolve('src')
    .resolve('main')
    .resolve('java')
    .resolve('org')
    .resolve('eolang')

Set<String> expected = Files.walk(classes)
    .filter(path -> path.toString().endsWith('.java'))
    .map(Path::getFileName)
    .map(Path::toString)
    .map(pathName -> pathName.replace('.java', '.class'))
    .collect(Collectors.toSet())
Set<String> actual = Files.walk(binaries)
    .filter(path -> path.toString().endsWith('.class'))
    .map(Path::getFileName)
    .map(Path::toString)
    .collect(Collectors.toSet())

if (!actual.containsAll(expected)) {
    for (String must : expected) {
        if (!actual.contains(must)) {
            log.error("Missing: ${must}")
        }
    }
    fail('Not all .java files were compiled to .class files')
}
