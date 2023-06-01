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
package org.eolang.maven.log;

import java.util.Enumeration;
import org.apache.log4j.Appender;
import org.apache.log4j.ConsoleAppender;
import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;
import org.apache.log4j.spi.LoggingEvent;

/**
 * Log4j logger appender.
 *
 * @since 0.30
 */
public final class CaptureLogsAppender extends ConsoleAppender {

    /**
     * Where to save logs from the appender.
     */
    private final Logs logs;

    /**
     * Ctor.
     * @param logs Where to save logs from the appender.
     */
    CaptureLogsAppender(final Logs logs) {
        this.logs = logs;
    }

    @Override
    public void append(final LoggingEvent event) {
        this.logs.append(this.getLayout().format(event));
    }

    /**
     * Initialize the appender.
     * Adds appender to the root logger.
     */
    void init() {
        final Logger logger = LogManager.getRootLogger();
        final Enumeration<?> appenders = logger.getAllAppenders();
        if (appenders.hasMoreElements()) {
            final Object next = appenders.nextElement();
            if (next instanceof ConsoleAppender) {
                this.setLayout(((Appender) next).getLayout());
            }
        }
        logger.addAppender(this);
        this.logs.waitForInit();
    }

    /**
     * Destroy the appender.
     * Removes appender from the root logger.
     */
    void destroy() {
        LogManager.getRootLogger().removeAppender(this);
    }
}
