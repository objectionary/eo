/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import java.util.concurrent.TimeoutException;
import org.junit.jupiter.api.extension.ExtensionContext;
import org.junit.jupiter.api.extension.TestExecutionExceptionHandler;
import org.opentest4j.TestAbortedException;

/**
 * A handler that turns the timeout of a test into an abort, so that a test
 * outliving the deadline set in junit-platform.properties is reported as
 * skipped instead of failed.
 * @since 0.73.3
 */
public final class Deadline implements TestExecutionExceptionHandler {

    @Override
    public void handleTestExecutionException(final ExtensionContext context,
        final Throwable error) throws Throwable {
        try {
            throw error;
        } catch (final TimeoutException ex) {
            throw new TestAbortedException(ex.getMessage());
        }
    }
}
