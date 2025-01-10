/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2025 Objectionary.com
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
package org.eolang;

import com.yegor256.Jaxec;
import com.yegor256.Jhome;
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
            MainTest.exec("--version"),
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
            MainTest.exec("--help"),
            Matchers.containsString("Usage: ")
        );
    }

    @Test
    void deliversCleanOutput() {
        MatcherAssert.assertThat(
            "Incorrect output when dataizing \"true\" object",
            MainTest.exec("org.eolang.true"),
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
            MainTest.exec("--verbose", "org.eolang.false"),
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
            MainTest.exec("--verbose", "as-bytes"),
            Matchers.allOf(
                Matchers.containsString("Loading class EOas_bytes"),
                Matchers.containsString("Can not find \"as-bytes\" object")
            )
        );
    }

    @Test
    void executesJvmFullRinWithAttributeCall() {
        MatcherAssert.assertThat(
            "Fails with the proper error message",
            MainTest.exec("--verbose", "string$as-bytes"),
            Matchers.allOf(
                Matchers.containsString("Loading class EOstring$EOas_bytes"),
                Matchers.containsString("Can not find \"string$as-bytes\" object")
            )
        );
    }

    @Test
    void executesJvmFullRunWithError() {
        MatcherAssert.assertThat(
            "Fails with the proper error message",
            MainTest.exec("--verbose", "org.eolang.io.stdout"),
            Matchers.containsString(
                "Error in \"Φ.org.eolang.io.stdout.φ.Δ\" "
            )
        );
    }

    @Test
    void executesWithObjectNotFoundException() {
        MatcherAssert.assertThat(
            "Fails with the proper error message",
            MainTest.exec("unavailable-name"),
            Matchers.containsString("Can not find \"unavailable-name\" object")
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
            AtCompositeTest.TO_ADD_MESSAGE,
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
            AtCompositeTest.TO_ADD_MESSAGE,
            reader.readLine().length(),
            Matchers.greaterThan(1)
        );
    }

    @Test
    void readsBytesCorrectly() {
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
            new ByteArrayInputStream(
                "··\uD835\uDD38➜Φ".getBytes(
                    StandardCharsets.UTF_8
                )
            ).read(),
            Matchers.greaterThan(0)
        );
    }

    private static String exec(final String... cmds) {
        final String stdout = new Jaxec(
            new Jhome().java().toString(),
            "-Dfile.encoding=UTF-8",
            "-Dsun.stdout.encoding=UTF-8",
            "-Dsun.stderr.encoding=UTF-8",
            "-cp",
            System.getProperty("java.class.path"),
            Main.class.getCanonicalName()
        ).with(cmds).withCheck(false).exec().stdout();
        return stdout.replaceFirst(
            String.format(
                "Picked up .*%s",
                System.lineSeparator()
            ),
            ""
        );
    }

}
