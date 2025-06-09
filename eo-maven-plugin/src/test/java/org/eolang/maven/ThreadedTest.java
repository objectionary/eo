/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Consumer;
import org.apache.log4j.Logger;
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

    @Test
    void reportsAllExceptionsInTheLogsOnFailure() {
        final List<String> logs = new ListOf<>();
        Exception thrown = null;
        try {
            new Threaded<>(
                new ListOf<>(1, 2, 3),
                input -> {
                    throw new RuntimeException(String.format("Failure on: %d", input));
                },
                logs::add
            ).total();
        } catch (final Exception exception) {
            thrown = exception;
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
