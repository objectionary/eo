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

import org.junit.jupiter.api.extension.AfterEachCallback;
import org.junit.jupiter.api.extension.BeforeEachCallback;
import org.junit.jupiter.api.extension.ExtensionContext;
import org.junit.jupiter.api.extension.ParameterContext;
import org.junit.jupiter.api.extension.ParameterResolver;

/**
 * JUnit extension to capture log messages.
 *
 * @since 0.30
 */
public final class CaptureLogsExtension implements
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
    public CaptureLogsExtension() {
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
