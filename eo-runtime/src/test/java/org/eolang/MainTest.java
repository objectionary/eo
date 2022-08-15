/*
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
package org.eolang;

import com.jcabi.log.VerboseProcess;
import java.io.ByteArrayOutputStream;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.Collection;
import java.util.LinkedList;
import org.cactoos.io.InputOf;
import org.cactoos.io.OutputTo;
import org.cactoos.io.TeeInput;
import org.cactoos.scalar.LengthOf;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Main}.
 *
 * @since 0.1
 */
public final class MainTest {

    @Test
    public void printsVersion() throws Exception {
        MatcherAssert.assertThat(
            MainTest.exec("--version"),
            Matchers.allOf(
                Matchers.containsString("."),
                Matchers.not(Matchers.containsString(" "))
            )
        );
    }

    @Test
    public void printsHelp() throws Exception {
        MatcherAssert.assertThat(
            MainTest.exec("--help"),
            Matchers.containsString("Usage: ")
        );
    }

    @Test
    public void jvmFullRun() throws Exception {
        MatcherAssert.assertThat(
            MainTest.exec("--verbose", "org.eolang.io.stdout", "Hello, dude!"),
            Matchers.allOf(
                Matchers.containsString("EOLANG"),
                Matchers.containsString("Hello, "),
                Matchers.containsString("\uD835\uDD3B( "),
                Matchers.containsString("string")
            )
        );
    }

    @Test
    public void jvmFullRunWithError() throws Exception {
        MatcherAssert.assertThat(
            MainTest.exec("--verbose", "org.eolang.io.stdout"),
            Matchers.containsString("Error at \"EOorg.EOeolang.EOio.EOstdout#text\" attribute")
        );
    }

    public static String exec(final String... cmds) throws Exception {
        final Collection<String> args = new LinkedList<>();
        args.add("java");
        args.add("-Dfile.encoding=utf-8");
        args.add("-cp");
        args.add(System.getProperty("java.class.path"));
        args.add(Main.class.getCanonicalName());
        args.addAll(Arrays.asList(cmds));
        final Process proc = new ProcessBuilder().command(
            args.toArray(new String[args.size()])
        ).redirectErrorStream(true).start();
        final ByteArrayOutputStream stdout = new ByteArrayOutputStream();
        try (VerboseProcess vproc = new VerboseProcess(proc)) {
            new LengthOf(
                new TeeInput(
                    new InputOf(vproc.stdoutQuietly()),
                    new OutputTo(stdout)
                )
            ).value();
        }
        return new String(stdout.toByteArray(), StandardCharsets.UTF_8)
            .replaceFirst("Picked up .*\n", "");
    }

}
