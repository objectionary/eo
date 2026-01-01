/**
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

/**
 * Check the internet connection.
 *
 * @return Is the internet connection available
 */
private static boolean online() {
    boolean online = true
    try {
        final URL url = new URL('http://www.google.com')
        final URLConnection conn = url.openConnection()
        conn.connect()
        conn.inputStream.close()
    } catch (final IOException ignored) {
        online = false
    }
    return online
}

String log = new File(basedir, 'build.log').text

[
        'org.eolang:eo-runtime:',
        ' unpacked to ',
        '6th Fibonacci number is 8',
        'BUILD SUCCESS',
].each { expectedLog -> assert log.contains(expectedLog) || !online() }

true
