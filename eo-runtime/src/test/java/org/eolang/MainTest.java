/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2021 Yegor Bugayenko
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

import java.io.ByteArrayOutputStream;
import java.nio.charset.StandardCharsets;
import java.util.logging.Formatter;
import java.util.logging.Handler;
import java.util.logging.LogRecord;
import java.util.logging.Logger;
import java.util.logging.StreamHandler;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Main}.
 *
 * @since 0.1
 */
public final class MainTest {

    private static final Formatter FMT = new Formatter() {
        @Override
        public String format(final LogRecord rec) {
            return String.format("%s%n", rec.getMessage());
        }
    };

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
    public void turnsOnDebugOutput() throws Exception {
        MatcherAssert.assertThat(
            MainTest.exec("--verbose", "org.eolang.io.stdout", "Hello, world!"),
            Matchers.containsString("EOLANG Runtime ")
        );
    }

    private static String exec(final String... args) throws Exception {
        final Logger log = Logger.getLogger("org.eolang");
        log.setUseParentHandlers(false);
        final ByteArrayOutputStream baos = new ByteArrayOutputStream();
        final Handler handler = new StreamHandler(baos, MainTest.FMT);
        log.addHandler(handler);
        Main.main(args);
        handler.flush();
        return new String(baos.toByteArray(), StandardCharsets.UTF_8);
    }

}
