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
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;
import org.apache.log4j.Appender;
import org.apache.log4j.WriterAppender;
import org.apache.log4j.spi.LoggingEvent;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Timeout;

/**
 * Tests of the log4j logger messages format.
 *
 * @since 0.28.11
 */
class LogFormatTest {

    @Test
    @Timeout(5)
    void printsFormattedMessage() {
        final org.apache.log4j.Logger logger = org.apache.log4j.Logger.getRootLogger();
        final Appender appender = logger.getAppender("CONSOLE");
        final MockAppender mock = new MockAppender(appender);
        logger.addAppender(mock);
        Logger.info(this, "Wake up, Neo...");
        final String expected =
            "^\\d{2}:\\d{2}:\\d{2} \\[INFO] org.eolang.maven.LogFormatTest: Wake up, Neo...\\R";
        MatcherAssert.assertThat(
            String.format("Expected message '%s', but log was:\n '%s'", expected, mock.raw()),
            mock.containsMessage(expected),
            Matchers.is(true)
        );
    }

    /**
     * Mock log4j adapter that intercepts all log messages.
     *
     * @since 0.28.11
     */
    private static final class MockAppender extends WriterAppender {
        /**
         * Real appender.
         */
        private final Appender console;

        /**
         * Last log message event.
         */
        private final BlockingQueue<LoggingEvent> events;

        /**
         * The main constructor.
         *
         * @param console Real log4j appender that we want to replace.
         */
        private MockAppender(final Appender console) {
            this.console = console;
            this.events = new LinkedBlockingQueue<>();
        }

        @Override
        public void append(final LoggingEvent event) {
            this.events.add(event);
            super.append(event);
        }

        /**
         * Check if any log message matches the regex.
         * @param regex The regex to match.
         * @return True if any log message matches the regex.
         */
        private boolean containsMessage(final String regex) {
            try {
                while (true) {
                    if (this.lastMessage().matches(regex)) {
                        return true;
                    }
                }
            } catch (final InterruptedException interrupt) {
                Thread.currentThread().interrupt();
                throw new IllegalStateException(interrupt);
            }
        }

        /**
         * Get the last log message.
         * @return The last log message.
         * @throws InterruptedException If interrupted.
         */
        private String lastMessage() throws InterruptedException {
            return this.console.getLayout().format(this.events.poll(5, TimeUnit.SECONDS));
        }

        /**
         * Get all log messages as a single string.
         * @return All log messages as a single string.
         */
        private String raw() {
            return this.events.stream().map(LoggingEvent::getRenderedMessage)
                .collect(Collectors.joining("\n"));
        }
    }
}
