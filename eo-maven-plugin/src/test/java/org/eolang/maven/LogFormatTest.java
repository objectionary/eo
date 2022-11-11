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
package org.eolang.maven;

import com.jcabi.log.Logger;
import java.util.concurrent.atomic.AtomicReference;
import org.apache.log4j.Appender;
import org.apache.log4j.WriterAppender;
import org.apache.log4j.spi.LoggingEvent;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

/**
 * Tests of the log4j logger messages format.
 *
 * @since 0.28.11
 */
class LogFormatTest {

    /**
     * Mock log4j appender that intercepts all log messages.
     */
    private MockAppender mock;

    @BeforeEach
    public void overrideLogAppender() {
        final org.apache.log4j.Logger logger = org.apache.log4j.Logger.getRootLogger();
        final Appender appender = logger.getAppender("CONSOLE");
        logger.removeAppender(appender);
        this.mock = new MockAppender(appender);
        logger.addAppender(this.mock);
    }

    @Test
    void printsFormattedMessage() {
        Logger.info(this, "Wake up, Neo...");
        MatcherAssert.assertThat(
            this.mock.lastLog(),
            Matchers.matchesPattern(
                "^\\d{2}:\\d{2}:\\d{2} \\[INFO] org.eolang.maven.LogFormatTest: Wake up, Neo...\\R"
            )
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
        private final AtomicReference<LoggingEvent> last;

        /**
         * The main constructor.
         *
         * @param console Real log4j appender that we want to replace.
         */
        private MockAppender(final Appender console) {
            this.console = console;
            this.last = new AtomicReference<>();
        }

        @Override
        public void append(final LoggingEvent event) {
            super.append(event);
            this.last.set(event);
        }

        /**
         * Formatted log message.
         *
         * @return Real log message.
         */
        private String lastLog() {
            return this.console.getLayout().format(this.last.get());
        }
    }
}
