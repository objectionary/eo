/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Causes}.
 *
 * @since 0.62.0
 */
final class CausesTest {

    @Test
    void readsAChainOfTwoMessagelessCauses() {
        MatcherAssert.assertThat(
            "a chain of two throwables without a message must still be read, but it wasnt",
            new Causes(
                new IllegalStateException(
                    null,
                    new IllegalStateException(
                        null, new IllegalStateException("something went wrong")
                    )
                )
            ),
            Matchers.contains("something went wrong")
        );
    }

    @Test
    void saysNothingForAMessagelessCause() {
        MatcherAssert.assertThat(
            "a throwable without a message must say nothing at all, but it spoke",
            new Causes(
                new IllegalStateException(
                    "outer failure",
                    new IllegalStateException(null, new IllegalStateException((String) null))
                )
            ),
            Matchers.contains("outer failure")
        );
    }

    @Test
    void dropsACauseRepeatedByAnEarlierOne() {
        MatcherAssert.assertThat(
            "a cause repeating a part of an earlier one must be dropped, but it stayed",
            new Causes(
                new IllegalStateException(
                    "the build failed because of a typo", new RuntimeException("a typo")
                )
            ),
            Matchers.contains("a typo")
        );
    }
}
