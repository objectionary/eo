/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import com.jcabi.log.Logger;
import org.junit.jupiter.api.extension.AfterEachCallback;
import org.junit.jupiter.api.extension.ExtensionContext;
import org.junit.jupiter.params.ParameterizedTest;

/**
 * Logs progress of parameterized tests to the console.
 * This class is used in test suites to indicate the progress of parameterized tests.
 * This class was created to mitigate the following issue:
 * <a href="https://github.com/objectionary/eo/issues/4833">4833</a>
 * @since 0.60
 */
final class LogProgress implements AfterEachCallback {

    @Override
    public void afterEach(final ExtensionContext context) {
        final Class<?> clazz = context.getTestClass().orElse(LogProgress.class);
        if (LogProgress.parameterized(context)) {
            Logger.info(
                clazz,
                "Parameterized test %s done",
                context.getDisplayName().lines().findFirst().orElse(context.getUniqueId())
            );
        }
    }

    /**
     * Checks if the test is parameterized.
     * @param context Extension context
     * @return True if parameterized test
     */
    private static boolean parameterized(final ExtensionContext context) {
        return context.getTestMethod()
            .map(method -> method.isAnnotationPresent(ParameterizedTest.class))
            .orElse(false);
    }
}
