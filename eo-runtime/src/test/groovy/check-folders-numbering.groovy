/**
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2025 Objectionary.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation directories (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

import java.nio.file.Path
import java.util.stream.Collectors

Path target = basedir.toPath().resolve('target/eo')
List<File> directories = target.toFile().listFiles((FileFilter) { File file -> file.directory })
List<String> allowed = [
    '1-parse',
    '2-shake',
    '4-pull',
    '5-resolve',
    '6-lint',
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
