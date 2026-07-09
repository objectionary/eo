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
 * Test case for {@link PhSafeTest}.
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
            "rethrows correctly",
            Assertions.assertThrows(
                EOerror.ExError.class,
                () -> new PhSafe(
                    new PhDefault() {
                        @Override
                        public byte[] delta() {
                            throw new IllegalArgumentException("oops");
                        }
                    }
                ).delta(),
                "throws correct class"
            ).messages(),
            Matchers.hasItems(
                Matchers.containsString("o.e.PhSafe#delta()"),
                Matchers.containsString("(j.l.IllegalArgumentException)"),
                Matchers.containsString("Error in \"?.Δ\" at unknown:0:0")
            )
        );
    }

    @Test
    void catchesRuntimeExceptionWithoutMessage() {
        Assertions.assertThrows(
            EOerror.ExError.class,
            () -> new PhSafe(
                new PhDefault() {
                    @Override
                    public byte[] delta() {
                        throw new RuntimeException();
                    }
                }
            ).delta(),
            "catches an exception with a null message"
        );
    }

    @Test
    void catchesAbstractExceptionWithoutMessage() {
        Assertions.assertThrows(
            EOerror.ExError.class,
            () -> new PhSafe(
                new PhDefault() {
                    @Override
                    public byte[] delta() {
                        throw new ExAbstract((String) null) {
                            /**
                             * Serialization identifier.
                             */
                            private static final long serialVersionUID =
                                6895797670679429822L;
                        };
                    }
                }
            ).delta(),
            "catches an abstract exception with a null message"
        );
    }

    @Test
    void rendersMultiLayeredErrorMessageCorrectly() {
        MatcherAssert.assertThat(
            "rethrows correctly",
            Assertions.assertThrows(
                EOerror.ExError.class,
                () -> new PhSafe(
                    new PhApplication(
                        new EOerror(),
                        "message",
                        new Data.ToPhi("oops")
                    )
                ).take("foo"),
                "throws correct class"
            ),
            Matchers.hasToString(
                Matchers.containsString("Δ = [0x6F6F7073-] = \"oops\"")
            )
        );
    }

    @Test
    void showsFileNameAndLineNumber() {
        MatcherAssert.assertThat(
            "shows file name and line number",
            new Dataized(
                Assertions.assertThrows(
                    EOerror.ExError.class,
                    () -> new PhSafe(
                        new PhDefault() {
                            @Override
                            public Phi take(final String name) {
                                throw new IllegalArgumentException("intentional error");
                            }
                        }
                    ).take("foo"),
                    "throws correct class"
                ).enclosure()
            ).take(String.class),
            Matchers.equalTo("intentional error")
        );
    }
}
