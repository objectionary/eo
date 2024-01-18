/*
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
package org.eolang.maven;

import com.jcabi.log.Logger;
import org.eolang.maven.log.CaptureLogs;
import org.eolang.maven.log.Logs;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;

/**
 * Tests of the log4j logger messages format.
 *
 * All log messages are written to System.out. System.out is a shared resource among all other
 * threads.  For this reason, we run tests in this class in the same thread (disabling parallelism).
 * This approach prevents log messages from other threads from interfering. Since all the tests in
 * this class are relatively fast, it does not significantly impact overall performance.
 * We disable parallelism by using the {@link Execution} annotation with
 * {@link ExecutionMode#SAME_THREAD}. DO NOT REMOVE THAT ANNOTATION!
 *
 * @since 0.28.11
 */
@Execution(ExecutionMode.SAME_THREAD)
@SuppressWarnings("JTCOP.RuleAllTestsHaveProductionClass")
class LogFormatTest {

    /**
     * Expected log message format.
     */
    private static final String FORMAT =
        "^\\d{2}:\\d{2}:\\d{2} \\[INFO] org.eolang.maven.LogFormatTest: Wake up, Neo...\\R";

    /**
     * Message to log.
     */
    private static final String MESSAGE = "Wake up, Neo...";

    @Test
    @CaptureLogs
    void printsFormattedMessage(final Logs out) {
        Logger.info(this, LogFormatTest.MESSAGE);
        final String actual = out.waitForMessage(LogFormatTest.MESSAGE);
        MatcherAssert.assertThat(
            String.format(
                "Actual log output is '%s', but expected pattern is: '%s'",
                actual,
                LogFormatTest.FORMAT
            ),
            actual,
            Matchers.matchesPattern(LogFormatTest.FORMAT)
        );
    }

    @Test
    void matchesCorrectly() {
        MatcherAssert.assertThat(
            "16:02:08 [INFO] org.eolang.maven.LogFormatTest: Wake up, Neo...\n",
            Matchers.matchesPattern(LogFormatTest.FORMAT)
        );
    }
}
