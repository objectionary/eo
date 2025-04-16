/**
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
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

[
        'target/eo/foreign.csv',
        'target/generated-sources/EOorg/EOeolang/EOexamples/EOapp.java',
        'target/eo/1-parse/org/eolang/examples/app.xmir',
        'target/eo/2-shake-steps/org/eolang/examples/app/00-not-empty-atoms.xml',
        'target/eo/2-shake/org/eolang/examples/app.xmir',
        'target/eo/6-pre/org/eolang/examples/app/01-classes.xml',
        'target/eo/7-transpile/org/eolang/examples/app.xmir',
].each { path -> assert new File(basedir, path).exists() }

[
        'target/classes/EOorg/EOeolang/EOexamples/EOapp.class',
        'target/eo/placed.json',
        'target/eo/4-pull/org/eolang/tuple.eo',
].each { path -> assert new File(basedir, path).exists() || !online() }

String log = new File(basedir, 'build.log').text

[
        'org.eolang:eo-runtime:',
        ' unpacked to ',
        '6th Fibonacci number is 8',
        'BUILD SUCCESS',
].each { expectedLog -> assert log.contains(expectedLog) || !online() }

true
