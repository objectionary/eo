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

/**
 * Here we check if EOe.class exists and if it contains "EOorg/EOeolang/EOint" which means that is
 * wasn't overwritten.
 */
if (online()) {
  File euler =  new File(basedir, 'target/classes/EOorg/EOeolang/EOmath/EOe.class')
  assert euler.exists()
  assert euler.text.contains('EOorg/EOeolang/EOint')
}

true
