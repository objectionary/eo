/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link PhSafe}.
 * @since 0.36.0
 */
final class PhSafeTest {

    @Test
    void delegatesTermToOrigin() {
        MatcherAssert.assertThat(
            "PhSafe must delegate φ-term to its origin, but it didnt",
            new PhSafe(new PhDefault(new byte[] {(byte) 0x01})).φTerm(),
            Matchers.equalTo("[D> 01]")
        );
    }

    @Test
    void savesLocationAfterCopying() {
        final Phi located = new PhSafe(new Data.ToPhi(0L), "foo", 123, 124, "qwerty", "fqn");
        MatcherAssert.assertThat(
            "saves location",
            located.copy().locator(),
            Matchers.equalTo(located.locator())
        );
    }

    @Test
    void catchesRuntimeException() {
        MatcherAssert.assertThat(
            "wraps a runtime exception into ExFailure with location and cause",
            Assertions.assertThrows(
                ExFailure.class,
                () -> new PhSafe(
                    new PhDefault() {
                        @Override
                        public byte[] delta() {
                            throw new IllegalArgumentException("oops");
                        }
                    }
                ).delta(),
                "was expected to fail with ExFailure"
            ).getMessage(),
            Matchers.allOf(
                Matchers.containsString("Error in \"?.Δ\" at unknown:0:0"),
                Matchers.containsString("oops")
            )
        );
    }

    @Test
    void catchesRuntimeExceptionWithoutMessage() {
        Assertions.assertThrows(
            ExFailure.class,
            () -> new PhSafe(
                new PhDefault() {
                    @Override
                    public byte[] delta() {
                        throw new IllegalStateException();
                    }
                }
            ).delta(),
            "catches an exception with a null message"
        );
    }

    @Test
    void catchesAbstractExceptionWithoutMessage() {
        Assertions.assertThrows(
            ExFailure.class,
            () -> new PhSafe(
                new PhDefault() {
                    @Override
                    public byte[] delta() {
                        throw new ExFailure((String) null, (Throwable) null);
                    }
                }
            ).delta(),
            "catches an abstract exception with a null message"
        );
    }

    @Test
    void rendersMultiLayeredErrorMessageCorrectly() {
        MatcherAssert.assertThat(
            "accumulates every layer's location around the original cause",
            Assertions.assertThrows(
                ExFailure.class,
                () -> new PhSafe(
                    new PhSafe(PhTerminator.withCause("oops"), "inner.eo", 1, 2),
                    "outer.eo", 3, 4
                ).delta(),
                "was expected to fail with ExFailure"
            ).getMessage(),
            Matchers.allOf(
                Matchers.containsString("at outer.eo:3:4"),
                Matchers.containsString("at inner.eo:1:2"),
                Matchers.containsString("oops")
            )
        );
    }

    @Test
    void showsFileNameAndLineNumber() {
        MatcherAssert.assertThat(
            "shows file name, line number and the original cause",
            Assertions.assertThrows(
                ExFailure.class,
                () -> new PhSafe(
                    new PhDefault() {
                        @Override
                        public Phi take(final String name) {
                            throw new IllegalArgumentException("intentional error");
                        }
                    },
                    "file.eo", 42, 7
                ).take("foo"),
                "was expected to fail with ExFailure"
            ).getMessage(),
            Matchers.allOf(
                Matchers.containsString("file.eo:42:7"),
                Matchers.containsString("intentional error")
            )
        );
    }
}
