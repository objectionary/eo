/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import EOorg.EOeolang.EOerror;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link PhSafeTest}.
 *
 * @since 0.36.0
 */
final class PhSafeTest {

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
                    new PhWith(
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
