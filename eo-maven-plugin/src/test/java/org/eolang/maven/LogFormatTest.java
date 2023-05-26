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
import java.util.Arrays;
import java.util.Optional;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.junitpioneer.jupiter.StdIo;
import org.junitpioneer.jupiter.StdOut;

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
class LogFormatTest {

    /**
     * Expected log message format.
     */
    private static final String FORMAT =
        "^\\d{2}:\\d{2}:\\d{2} \\[INFO] org.eolang.maven.LogFormatTest: Wake up, Neo...";

    /**
     * Message to log.
     */
    private static final String MESSAGE = "Wake up, Neo...";

    @StdIo
    @Test
    void printsFormattedMessage(final StdOut out) {
        Logger.info(this, LogFormatTest.MESSAGE);
        final String actual = LogFormatTest.waitForMessage(out);
        MatcherAssert.assertThat(
            String.format(
                "Expected message '%s', but log was:\n '%s'",
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
            "16:02:08 [INFO] org.eolang.maven.LogFormatTest: Wake up, Neo...",
            Matchers.matchesPattern(LogFormatTest.FORMAT)
        );
    }

    private static String waitForMessage(final StdOut out) {
        try {
            return Executors.newSingleThreadExecutor().submit(
                () -> {
                    while (true) {
                        final Optional<String> message = Arrays.stream(out.capturedLines())
                            .filter(s -> s.contains(LogFormatTest.MESSAGE))
                            .findFirst();
                        if (message.isPresent()) {
                            return message.get();
                        }
                    }
                }
            ).get(10, TimeUnit.SECONDS);
        } catch (final InterruptedException exception) {
            Thread.currentThread().interrupt();
            throw new IllegalStateException(
                String.format(
                    "Waiting thread was interrupted, can't read '%s' msg",
                    LogFormatTest.MESSAGE
                ),
                exception
            );
        } catch (final ExecutionException exception) {
            throw new IllegalStateException(
                String.format("Some problem happened, can't read '%s' msg", LogFormatTest.MESSAGE),
                exception
            );
        } catch (final TimeoutException exception) {
            throw new IllegalStateException(
                String.format("Timeout limit exceed to read msg %s", LogFormatTest.MESSAGE),
                exception
            );
        }
    }
}
