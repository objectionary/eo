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
package org.eolang.maven.log;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.util.Enumeration;
import org.apache.log4j.Appender;
import org.apache.log4j.ConsoleAppender;
import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;
import org.apache.log4j.spi.LoggingEvent;
import org.junit.jupiter.api.extension.AfterEachCallback;
import org.junit.jupiter.api.extension.BeforeEachCallback;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.extension.ExtensionContext;
import org.junit.jupiter.api.extension.ParameterContext;
import org.junit.jupiter.api.extension.ParameterResolver;

/**
 * Captured logs annotation for tests.
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.METHOD, ElementType.ANNOTATION_TYPE})
@ExtendWith(CaptureLogs.CaptureLogsExtension.class)
@SuppressWarnings("JTCOP.RuleAllTestsHaveProductionClass")
public @interface CaptureLogs {

    /**
     * JUnit extension to capture log messages.
     *
     * @since 0.30
     */
    final class CaptureLogsExtension implements
        ParameterResolver, BeforeEachCallback, AfterEachCallback {

        /**
         * Logs.
         */
        private final Logs logs;

        /**
         * Appender.
         */
        private final CaptureLogsAppender appender;

        /**
         * Ctor.
         */
        CaptureLogsExtension() {
            this(new Logs());
        }

        /**
         * Ctor.
         * @param logs Where to save logs from the appender.
         */
        private CaptureLogsExtension(final Logs logs) {
            this(logs, new CaptureLogsAppender(logs));
        }

        /**
         * Ctor.
         * @param logs Where to save logs from the appender.
         * @param appender Appender to use.
         */
        private CaptureLogsExtension(final Logs logs, final CaptureLogsAppender appender) {
            this.logs = logs;
            this.appender = appender;
        }

        @Override
        public void beforeEach(final ExtensionContext context) {
            this.appender.init();
        }

        @Override
        public void afterEach(final ExtensionContext context) {
            this.appender.destroy();
        }

        @Override
        public boolean supportsParameter(
            final ParameterContext param,
            final ExtensionContext extension
        ) {
            return param.getParameter().getType() == Logs.class;
        }

        @Override
        public Object resolveParameter(
            final ParameterContext param,
            final ExtensionContext extension
        ) {
            return this.logs;
        }
    }

    /**
     * Log4j logger appender.
     *
     * @since 0.30
     */
    final class CaptureLogsAppender extends ConsoleAppender {

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
}
