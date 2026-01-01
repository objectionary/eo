/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import com.yegor256.Jaxec;
import com.yegor256.Jhome;
import com.yegor256.Result;
import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.nio.channels.Channels;
import java.nio.charset.StandardCharsets;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Main}.
 *
 * @since 0.1
 */
@SuppressWarnings("PMD.TooManyMethods")
final class MainTest {

    @BeforeAll
    static void checkHeapSize() {
        final int gbs = 2;
        MatcherAssert.assertThat(
            String.format(
                "The maximum heap size is not big enough, some tests may fail, try to set JAVA_OPTS environment variable to '-Xmx%dg'",
                gbs
            ),
            Runtime.getRuntime().maxMemory(),
            Matchers.greaterThanOrEqualTo(1024L * 1024 * 1024 * gbs)
        );
    }

    @Test
    void printsVersion() {
        MatcherAssert.assertThat(
            "Prints its own version properly",
            MainTest.stderr("--version"),
            Matchers.allOf(
                Matchers.containsString("."),
                Matchers.not(Matchers.containsString(" "))
            )
        );
    }

    @Test
    void printsHelp() {
        MatcherAssert.assertThat(
            "Prints help summary properly",
            MainTest.stderr("--help"),
            Matchers.containsString("Usage: ")
        );
    }

    @Test
    void deliversCleanOutput() {
        MatcherAssert.assertThat(
            "Incorrect output when dataizing \"true\" object",
            MainTest.stderr("org.eolang.true"),
            Matchers.stringContainsInOrder(
                String.format("%n---%n"),
                "true",
                String.format("%n")
            )
        );
    }

    @Test
    void executesJvmFullRun() {
        MatcherAssert.assertThat(
            "Incorrect verbose output when dataizing \"false\" object",
            MainTest.stderr(Main.VERBOSE, "org.eolang.false"),
            Matchers.allOf(
                Matchers.containsString("EOLANG"),
                Matchers.containsString("false")
            )
        );
    }

    @Test
    void executesJvmFullRunWithDashedObject() {
        MatcherAssert.assertThat(
            "Fails with the proper error message",
            MainTest.stderr(Main.VERBOSE, "as-bytes"),
            Matchers.containsString("Couldn't find object 'Φ.as-bytes'")
        );
    }

    @Test
    void executesJvmFullRunWithAttributeCall() {
        MatcherAssert.assertThat(
            "Fails with the proper error message",
            MainTest.stderr(Main.VERBOSE, "string$as-bytes"),
            Matchers.containsString("Couldn't find object 'Φ.string$as-bytes'")
        );
    }

    @Test
    void executesJvmFullRunWithError() {
        MatcherAssert.assertThat(
            "Fails with the proper error message",
            MainTest.stderr(Main.VERBOSE, "org.eolang.io.stdout"),
            Matchers.containsString(
                "Error in \"Φ.org.eolang.io.stdout.φ.Δ\" "
            )
        );
    }

    @Test
    void executesWithObjectNotFoundException() {
        MatcherAssert.assertThat(
            "Fails with the proper error message",
            MainTest.stderr("unavailable-name"),
            Matchers.containsString("Couldn't find object 'Φ.unavailable-name'")
        );
    }

    @Test
    void readsStreamCorrectly() throws IOException {
        final BufferedReader reader = new BufferedReader(
            Channels.newReader(
                Channels.newChannel(
                    new ByteArrayInputStream(
                        ">> ··\uD835\uDD38('text' for EOorgEOio.EOstdoutν2) ➜ ΦSFN".getBytes(
                            StandardCharsets.UTF_8
                        )
                    )
                ),
                StandardCharsets.UTF_8.name()
            )
        );
        MatcherAssert.assertThat(
            "Reading stream should produce a non-empty line, but it didn't",
            reader.readLine().length(),
            Matchers.greaterThan(0)
        );
    }

    @Test
    void readsSimpleStreamCorrectly() throws IOException {
        final BufferedReader reader = new BufferedReader(
            Channels.newReader(
                Channels.newChannel(
                    new ByteArrayInputStream(
                        "abc".getBytes(
                            StandardCharsets.UTF_8
                        )
                    )
                ),
                StandardCharsets.UTF_8.name()
            )
        );
        MatcherAssert.assertThat(
            "Reading stream should produce a line longer then 1 character, but it didn't",
            reader.readLine().length(),
            Matchers.greaterThan(1)
        );
    }

    @Test
    void readsBytesCorrectly() {
        MatcherAssert.assertThat(
            "Reading stream should produce a byte of next character, but it didn't",
            new ByteArrayInputStream(
                "··\uD835\uDD38➜Φ".getBytes(
                    StandardCharsets.UTF_8
                )
            ).read(),
            Matchers.greaterThan(0)
        );
    }

    @Test
    void printsFullStacktraceIfVerboseIsEnabled() {
        MatcherAssert.assertThat(
            "Prints full stacktrace when verbose mode is enabled",
            MainTest.stderr("--verbose", "org.eolang.examples.app"),
            Matchers.allOf(
                Matchers.containsString("at org.eolang.PhPackage.loadPhi"),
                Matchers.containsString("at org.eolang.PhPackage.take"),
                Matchers.containsString("at org.eolang.Main.run")
            )
        );
    }

    private static String stderr(final String... cmds) {
        return MainTest.exec(cmds).stderr();
    }

    private static Result exec(final String... cmds) {
        return new Jaxec(
            new Jhome().java().toString(),
            "-Dfile.encoding=UTF-8",
            "-Dsun.stdout.encoding=UTF-8",
            "-Dsun.stderr.encoding=UTF-8",
            "-cp",
            System.getProperty("java.class.path"),
            Main.class.getCanonicalName()
        ).with(cmds).withCheck(false).exec();
    }

}
