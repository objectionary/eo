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
    void takesNavValueOnSuccess() {
        MatcherAssert.assertThat(
            "takeNav must return attribute value",
            new Dataized(
                PhSafe.takeNav(
                    new PhSafeTest.ChainLink(
                        "x",
                        new PhDefault(new BytesOf(7.0).take())
                    ),
                    "x"
                )
            ).asNumber(),
            Matchers.equalTo(7.0)
        );
    }

    @Test
    void returnsNavErrorOnFailure() {
        MatcherAssert.assertThat(
            "takeNav must return error enclosure",
            PhSafe.takeNav(
                new PhSafeTest.BrokenLink(PhSafeTest.error("it is broken")),
                "x"
            ).forma(),
            Matchers.containsString("error")
        );
    }

    @Test
    void stopsNavLongChainOnMiddleFailure() {
        MatcherAssert.assertThat(
            "middle link must return error object",
            PhSafe.takeNav(
                PhSafe.takeNav(
                    new PhSafeTest.ChainLink(
                        "mid",
                        new PhSafeTest.BrokenMiddle(PhSafeTest.error("middle failed"))
                    ),
                    "mid"
                ),
                "inner"
            ).forma(),
            Matchers.containsString("error")
        );
    }

    @Test
    void succeedsNavLongChainWhenAllOk() {
        MatcherAssert.assertThat(
            "full chain must dataize to leaf value",
            new Dataized(
                PhSafe.takeNav(
                    PhSafe.takeNav(
                        new PhSafeTest.ChainLink(
                            "mid",
                            new PhSafeTest.ChainLink(
                                "inner",
                                new PhDefault(new BytesOf(42.0).take())
                            )
                        ),
                        "mid"
                    ),
                    "inner"
                )
            ).asNumber(),
            Matchers.equalTo(42.0)
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

    /**
     * Build error enclosure without {@link Data.ToPhi}.
     * @param text Error text
     * @return Error object
     */
    private static Phi error(final String text) {
        final Phi enc = new EOerror().copy();
        enc.put(
            "message",
            new PhDefault(text.getBytes(java.nio.charset.StandardCharsets.UTF_8))
        );
        return enc;
    }

    /**
     * One link in a dispatch chain.
     * @since 0.57
     */
    private static final class ChainLink extends PhDefault {

        /**
         * Ctor.
         * @param name Attribute name
         * @param next Next object in chain
         */
        ChainLink(final String name, final Phi next) {
            super(
                new Attrs(
                    new Attr(
                        name,
                        new AtComposite(null, self -> next)
                    )
                )
            );
        }
    }

    /**
     * Link that fails on take.
     * @since 0.57
     */
    private static final class BrokenLink extends PhDefault {

        /**
         * Ctor.
         * @param err Error enclosure
         */
        BrokenLink(final Phi err) {
            super(
                new Attrs(
                    new Attr(
                        "x",
                        new AtComposite(
                            null,
                            self -> {
                                throw new EOerror.ExError(err);
                            }
                        )
                    )
                )
            );
        }
    }

    /**
     * Middle link that fails on inner take.
     * @since 0.57
     */
    private static final class BrokenMiddle extends PhDefault {

        /**
         * Ctor.
         * @param err Error enclosure
         */
        BrokenMiddle(final Phi err) {
            super(
                new Attrs(
                    new Attr(
                        "inner",
                        new AtComposite(
                            null,
                            self -> {
                                throw new EOerror.ExError(err);
                            }
                        )
                    )
                )
            );
        }
    }
}
