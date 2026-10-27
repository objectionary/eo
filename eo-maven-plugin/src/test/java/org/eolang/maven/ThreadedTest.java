/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.util.Collections;
import java.util.List;
import java.util.stream.Stream;
import org.cactoos.Fallback;
import org.cactoos.list.ListOf;
import org.cactoos.scalar.ScalarWithFallback;
import org.cactoos.scalar.Unchecked;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

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

    @ParameterizedTest
    @MethodSource("fatalErrors")
    void namesTheFailingSourceForErrors(final Error failure) {
        final List<Object> events = Collections.synchronizedList(new ListOf<>());
        MatcherAssert.assertThat(
            "The diagnostic must identify the failing source and retain its cause",
            new Unchecked<>(
                new ScalarWithFallback<>(
                    () -> {
                        new Threaded<>(
                            new ListOf<>("a.eo", "b.eo"),
                            source -> {
                                if ("b.eo".equals(source)) {
                                    throw failure;
                                }
                                return 1;
                            },
                            events::add
                        ).total();
                        return events;
                    },
                    new Fallback.From<>(
                        Exception.class,
                        cause -> {
                            events.add(
                                Stream.<Throwable>iterate(
                                    cause, item -> item != null, Throwable::getCause
                                ).anyMatch(Matchers.sameInstance(failure)::matches)
                            );
                            return events;
                        }
                    )
                )
            ).value(),
            Matchers.<Object>contains(
                "Failed to process \"b.eo\" (java.lang.String)", true
            )
        );
    }

    private static Stream<Error> fatalErrors() {
        return Stream.of(
            new StackOverflowError("Synthetic stack-overflow fixture"),
            new OutOfMemoryError("Synthetic out-of-memory fixture")
        );
    }
}
