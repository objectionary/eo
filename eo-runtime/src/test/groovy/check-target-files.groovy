/**
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2024 Objectionary.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
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

List<String> expected = [
  'eo/1-parse/bytes.xmir',
  'eo/1-parse/fs/dir.xmir',
  'eo/2-optimize/error.xmir',
  'eo/2-optimize/sys/os.xmir',
  'eo/6-lint/go.xmir',
  'eo/8-transpile/malloc.xmir',
  'eo/phi/number.phi',
  'eo-test/1-parse/bool-tests.xmir',
  'eo-test/2-optimize/go-tests.xmir',
  'eo-test/6-lint/dataized-tests.xmir',
  'eo-test/8-transpile/runtime-tests.xmir',
  'eo-test/phi/number-tests.phi',
]
for (path in expected) {
  if (new File(path).exists()) {
    fail(String.format("The file '%s' is not present", path))
  }
}
