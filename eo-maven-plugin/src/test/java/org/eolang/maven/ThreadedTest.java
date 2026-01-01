/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.util.Collections;
import java.util.List;
import org.cactoos.list.ListOf;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Tests for {@link Threaded}.
 *
 * @since 0.56.5
 */
final class ThreadedTest {

    /**
     * Logs all exceptions.
     * @checkstyle IllegalCatchCheck (25 lines)
     * @checkstyle MethodBodyCommentsCheck (25 lines)
     */
    @SuppressWarnings({"PMD.AvoidCatchingGenericException", "PMD.AvoidThrowingRawExceptionTypes"})
    @Test
    void logsAllExceptionsInTheLogsOnFailure() {
        final List<String> logs = Collections.synchronizedList(new ListOf<>());
        try {
            new Threaded<>(
                new ListOf<>(1, 2, 3),
                input -> {
                    throw new RuntimeException(String.format("Failure on: %d", input));
                },
                logs::add
            ).total();
        } catch (final Exception ignored) {
            // Swallow the exception in order to do asserts
        }
        MatcherAssert.assertThat(
            "Logs don't have all failure messages, but they should",
            logs,
            Matchers.hasItems(
                Matchers.containsString("Failed to process \"1\""),
                Matchers.containsString("Failed to process \"2\""),
                Matchers.containsString("Failed to process \"3\"")
            )
        );
    }
}
