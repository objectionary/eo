/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.util.concurrent.TimeUnit;
import org.apache.maven.plugin.MojoFailureException;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Timeout;

/**
 * Test case for {@link Deadline}.
 * @since 0.62.0
 */
final class DeadlineTest {

    @Test
    void letsAFastBodyFinish() {
        Assertions.assertDoesNotThrow(
            () -> new Deadline(this, 5, false).spent(() -> "done"),
            "A body that finishes well within the deadline must not fail"
        );
    }

    @Test
    void refusesANegativeDeadline() {
        MatcherAssert.assertThat(
            "a negative timeout must be reported as a configuration mistake, not as a timeout",
            Assertions.assertThrows(
                IllegalArgumentException.class,
                () -> new Deadline(this, -1L, false).spent(() -> "never"),
                "a negative timeout was expected to be refused"
            ).getMessage(),
            Matchers.containsString("must not be negative")
        );
    }

    @Test
    @Timeout(30)
    void interruptsAHungBodyPromptlyOnTimeout() {
        final long start = System.nanoTime();
        Assertions.assertThrows(
            MojoFailureException.class,
            () -> new Deadline(this, 1, false).spent(
                () -> {
                    Thread.sleep(Long.MAX_VALUE);
                    return null;
                }
            ),
            "A body that never returns must fail with a MojoFailureException once the deadline passes"
        );
        MatcherAssert.assertThat(
            "The hung body must be interrupted shortly after the deadline, not left running for a long time",
            TimeUnit.NANOSECONDS.toSeconds(System.nanoTime() - start),
            Matchers.lessThan(30L)
        );
    }
}
