/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2023 Objectionary.com
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
package org.eolang.maven;

import com.jcabi.log.Logger;
import java.io.IOException;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.junitpioneer.jupiter.StdIo;
import org.junitpioneer.jupiter.StdOut;
import org.junitpioneer.jupiter.WritesStdIo;

/**
 * Tests of the log4j logger messages format.
 *
 * @since 0.28.11
 */
@Execution(ExecutionMode.SAME_THREAD)
class LogFormatTest {

    /**
     * Expected log message format.
     */
    private static final String FORMAT =
        "^\\d{2}:\\d{2}:\\d{2} \\[INFO] org.eolang.maven.LogFormatTest: Wake up, Neo...";

    @Test
    @StdIo
    @WritesStdIo
    void printsFormattedMessage(final StdOut out) throws IOException {
        final String message = "Wake up, Neo...";
        Logger.info(this, message);
        out.flush();
        final String[] lines = out.capturedLines();
        MatcherAssert.assertThat(
            lines.length,
            Matchers.greaterThan(0)
        );
        MatcherAssert.assertThat(lines[0], Matchers.containsString(message));
        MatcherAssert.assertThat(
            String.format(
                "Expected message '%s', but log was:\n '%s'",
                lines[0],
                LogFormatTest.FORMAT
            ),
            lines[0],
            Matchers.matchesPattern(LogFormatTest.FORMAT)
        );
    }

    @Test
    void matchesCorrectly() {
        MatcherAssert.assertThat(
            "16:02:08 [INFO] org.eolang.maven.LogFormatTest: Wake up, Neo...",
            Matchers.matchesPattern(LogFormatTest.FORMAT)
        );
    }
}
