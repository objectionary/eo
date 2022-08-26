/**
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2022 Objectionary.com
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
private static boolean netIsAvailable() {
    boolean available = true;
    try {
        final URL url = new URL("http://www.google.com");
        final URLConnection conn = url.openConnection();
        conn.connect();
        conn.getInputStream().close();
    } catch (final IOException ignored) {
        available = false;
    }
    return available;
}

[
  'target/eo/foreign.csv',
  'target/generated-sources/EOorg/EOeolang/EOexamples/EOapp.java',
  'target/eo/01-parse/org/eolang/examples/app.xmir',
  'target/eo/02-steps/org/eolang/examples/app/00-not-empty-atoms.xml',
  'target/eo/03-optimize/org/eolang/examples/app.xmir',
  'target/eo/05-pre/org/eolang/examples/app/00-pre-classes.xml',
  'target/eo/06-transpile/org/eolang/examples/app.xmir',
].each { assert new File(basedir, it).exists()}

[
  'target/classes/EOorg/EOeolang/EOexamples/EOapp.class',
  'target/eo/placed.csv',
  'target/eo/04-pull/org/eolang/array.eo',
].each { assert new File(basedir, it).exists() ||  !netIsAvailable() }

String log = new File(basedir, 'build.log').text

[
  '--- eo-maven-plugin:',
  'org.eolang unpacked to eo-runtime',
  '6th Fibonacci number is 8',
  'BUILD SUCCESS',
].each { assert log.contains(it) || !netIsAvailable() }

true
