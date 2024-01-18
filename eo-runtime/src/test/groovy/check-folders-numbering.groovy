import java.util.stream.Collectors

/**
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2024 Objectionary.com
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

target = basedir.toPath().resolve("target").resolve("eo")
List<File> directories = target.toFile().listFiles(new FileFilter() {
  @Override
  boolean accept(final File pathname) {
    return pathname.isDirectory()
  }
})
List<String> allowed = [
  '1-parse',
  '2-optimize',
  '3-shake',
  '4-pull',
  '5-resolve',
  '6-verify',
  '7-pre',
  '8-transpile',
]
List<File> allowedDirs = allowed.stream()
  .map { target.resolve(it).toFile() }
  .collect(Collectors.toList())
for (dir in directories) {
  if (!allowedDirs.contains(dir)) {
    fail(String.format("The directory '%s' is not expected to be here. Allowed directories %s", dir.name, allowed));
  }
}
true
