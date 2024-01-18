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

/**
 * Check the internet connection.
 *
 * @return Is the internet connection available
 */
private static boolean online() {
  boolean online = true
  try {
    final URL url = new URL("http://www.google.com")
    final URLConnection conn = url.openConnection()
    conn.connect()
    conn.inputStream.close()
  } catch (final IOException ignored) {
    online = false
  }
  return online
}

[
  'target/eo/foreign.csv',
  'target/generated-sources/EOorg/EOeolang/EOexamples/EOapp.java',
  'target/eo/1-parse/org/eolang/examples/app.xmir',
  'target/eo/2-optimization-steps/org/eolang/examples/app/00-not-empty-atoms.xml',
  'target/eo/2-optimize/org/eolang/examples/app.xmir',
  'target/eo/6-pre/org/eolang/examples/app/01-classes.xml',
  'target/eo/7-transpile/org/eolang/examples/app.xmir',
  'target/eo/sodg/org/eolang/error.sodg',
  'target/eo/sodg/org/eolang/error.sodg.xe',
  'target/eo/sodg/org/eolang/error.sodg.graph.xml',
  'target/eo/sodg/org/eolang/error.sodg.dot',
].each { assert new File(basedir, it).exists() }

[
  'target/classes/EOorg/EOeolang/EOexamples/EOapp.class',
  'target/eo/placed.json',
  'target/eo/4-pull/org/eolang/tuple.eo',
].each { assert new File(basedir, it).exists() || !online() }

String log = new File(basedir, 'build.log').text

[
  'org.eolang:eo-runtime:',
  ' unpacked to ',
  '6th Fibonacci number is 8',
  'BUILD SUCCESS',
].each { assert log.contains(it) || !online() }

true
