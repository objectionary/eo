/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.util.Collections;
import java.util.List;
import org.cactoos.Fallback;
import org.cactoos.list.ListOf;
import org.cactoos.scalar.ScalarWithFallback;
import org.cactoos.scalar.Unchecked;
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
    void logsAllExceptionsInTheLogsOnFailure() {
        final List<String> logs = Collections.synchronizedList(new ListOf<>());
        MatcherAssert.assertThat(
            "Logs dont have all failure messages, but they should",
            new Unchecked<>(
                new ScalarWithFallback<>(
                    () -> {
                        new Threaded<>(
                            new ListOf<>(1, 2, 3),
                            input -> {
                                throw new IllegalStateException(
                                    String.format("Failure on: %d", input)
                                );
                            },
                            logs::add
                        ).total();
                        return logs;
                    },
                    new Fallback.From<>(Exception.class, ex -> logs)
                )
            ).value(),
            Matchers.hasItems(
                Matchers.containsString("Failed to process \"1\""),
                Matchers.containsString("Failed to process \"2\""),
                Matchers.containsString("Failed to process \"3\"")
            )
        );
    }
}
