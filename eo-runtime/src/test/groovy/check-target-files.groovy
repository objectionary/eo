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

import java.nio.file.Paths

List<String> expected = [
  'eo-foreign.csv',
  'eo/1-parse/org/eolang/bytes.xmir',
  'eo/1-parse/org/eolang/fs/dir.xmir',
  'eo/2-optimize/org/eolang/error.xmir',
  'eo/2-optimize/org/eolang/sys/os.xmir',
  'eo/6-lint/org/eolang/go.xmir',
  'eo/8-transpile/org/eolang/malloc.xmir',
  'eo/phi/org/eolang/number.phi',
  'eo/unphi/org/eolang/number.xmir',
  'eo-test/1-parse/org/eolang/bool-tests.xmir',
  'eo-test/2-optimize/org/eolang/go-tests.xmir',
  'eo-test/6-lint/org/eolang/dataized-tests.xmir',
  'eo-test/8-transpile/org/eolang/runtime-tests.xmir',
  'eo-test/phi/org/eolang/number-tests.phi',
  'eo-test/unphi/org/eolang/number-tests.xmir',
  'generated-sources/EOorg/EOeolang/EOdataized.java',
  'generated-sources/EOorg/EOeolang/EOnet/EOsocket.java',
  'generated-test-sources/EOorg/EOeolang/EOand_with_zeroTest.java',
  'classes/EO-SOURCES/org/eolang/false.eo',
]

for (path in expected) {
    File f = Paths.get('eo-runtime/target').resolve(path).toFile()
    if (!f.exists()) {
        fail("The file '${f}' is not present")
    }
}
