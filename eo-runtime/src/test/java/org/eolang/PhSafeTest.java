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
 *
 * @since 0.36.0
 */
final class PhSafeTest {

    @Test
    void delegatesTermToOrigin() {
        MatcherAssert.assertThat(
            "PhSafe must delegate φ-term to its origin, but it didnt",
            new PhSafe(new PhDefault(new byte[] {(byte) 0x01})).φTerm(),
            Matchers.equalTo("[D> 01-]")
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
    void keepsProtectingANormalizedObject() {
        MatcherAssert.assertThat(
            "a failure of a normalized object must still name its location, but it didnt",
            Assertions.assertThrows(
                ExFailure.class,
                () -> new PhSafe(
                    new PhDefault() {
                        @Override
                        public Phi normalized() {
                            return new PhDefault() {
                                @Override
                                public byte[] delta() {
                                    throw new IllegalArgumentException("boom");
                                }
                            };
                        }
                    },
                    "example.eo", 3, 5
                ).normalized().delta(),
                "was expected to fail with ExFailure"
            ).getMessage(),
            Matchers.allOf(
                Matchers.containsString("at example.eo:3:5"),
                Matchers.containsString("boom")
            )
        );
    }

    @Test
    void passesTheLoopSignalThrough() {
        Assertions.assertThrows(
            ExAgain.class,
            () -> new PhSafe(new PhAgain(new Data.ToPhi(1L))).delta(),
            "the signal of a tail call must pass through untouched, but it was wrapped"
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
                    new PhSafe(new PhTerminator(new Data.ToPhi("oops")), "inner.eo", 1, 2),
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
    void catchesNonAtomOriginOnLambda() {
        MatcherAssert.assertThat(
            "wraps a non-atom origin's cast failure into ExFailure instead of letting it escape",
            Assertions.assertThrows(
                ExFailure.class,
                () -> new PhSafe(new PhDefault()).lambda(),
                "was expected to fail with ExFailure"
            ).getMessage(),
            Matchers.containsString(".λ")
        );
    }

    @Test
    void letsInterruptedThrough() {
        MatcherAssert.assertThat(
            "an interrupt must keep its own message, but it was wrapped",
            Assertions.assertThrows(
                ExInterrupted.class,
                () -> new PhSafe(
                    new PhDefault() {
                        @Override
                        public byte[] delta() {
                            throw new ExInterrupted("the thread must stop");
                        }
                    },
                    "file.eo", 42, 7
                ).delta(),
                "was expected to fail with ExInterrupted"
            ).getMessage(),
            Matchers.equalTo("the thread must stop")
        );
    }

    @Test
    void letsErrorThrough() {
        MatcherAssert.assertThat(
            "a JVM Error must keep its own type, but it was wrapped",
            Assertions.assertThrows(
                StackOverflowError.class,
                () -> new PhSafe(
                    new PhDefault() {
                        @Override
                        public byte[] delta() {
                            throw new StackOverflowError("the stack is exhausted");
                        }
                    },
                    "file.eo", 42, 7
                ).delta(),
                "was expected to fail with StackOverflowError"
            ).getMessage(),
            Matchers.equalTo("the stack is exhausted")
        );
    }

    @Test
    void letsErrorThroughFromAtom() {
        MatcherAssert.assertThat(
            "a JVM Error from an atom must keep its own type, but it was wrapped",
            Assertions.assertThrows(
                StackOverflowError.class,
                () -> new PhSafe(new PhSafeTest.Broken(), "file.eo", 3, 5).lambda(),
                "was expected to fail with StackOverflowError"
            ).getMessage(),
            Matchers.equalTo("the stack is exhausted")
        );
    }

    @Test
    void doesNotLetRecoveredInterceptError() {
        final EOrecovered recovered = new EOrecovered();
        recovered.put(
            "value",
            new PhSafe(
                new PhDefault() {
                    @Override
                    public Phi normalized() {
                        throw new OutOfMemoryError("the heap is exhausted");
                    }
                },
                "file.eo", 42, 7
            )
        );
        recovered.put("alternative", new Data.ToPhi(42L));
        Assertions.assertThrows(
            OutOfMemoryError.class,
            recovered::lambda,
            "recovered must not intercept a JVM Error, but it did"
        );
    }

    @Test
    void doesNotLetRecoveredInterceptInterrupted() {
        final EOrecovered recovered = new EOrecovered();
        recovered.put(
            "value",
            new PhSafe(
                new PhDefault() {
                    @Override
                    public Phi normalized() {
                        throw new ExInterrupted("the thread must stop");
                    }
                },
                "file.eo", 42, 7
            )
        );
        recovered.put("alternative", new Data.ToPhi(42L));
        Assertions.assertThrows(
            ExInterrupted.class,
            recovered::lambda,
            "recovered must not intercept an interrupt, but it did"
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

    /**
     * An atom that fails with a JVM error.
     *
     * @since 0.60.0
     */
    private static final class Broken extends PhDefault implements Atom {

        @Override
        public Phi lambda() {
            throw new StackOverflowError("the stack is exhausted");
        }
    }
}
