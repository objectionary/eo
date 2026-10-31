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
 * @since 0.71.0
 */
final class CausesTest {

    @Test
    void skipsConsecutiveMissingMessages() {
        MatcherAssert.assertThat(
            "message-less wrappers must not hide the real failure",
            new Causes(
                new RuntimeException(
                    null, new RuntimeException(null, new RuntimeException("root failure"))
                )
            ),
            Matchers.contains("root failure")
        );
    }

    @Test
    void skipsMissingMessagesBetweenDistinctFailures() {
        MatcherAssert.assertThat(
            "all absent messages must be removed without discarding distinct reasons",
            new Causes(
                new RuntimeException(
                    "outer failure",
                    new RuntimeException(
                        null, new RuntimeException(null, new RuntimeException("root"))
                    )
                )
            ),
            Matchers.contains("outer failure", "root")
        );
    }

    @Test
    void omitsAnEntirelyMessageLessChain() {
        MatcherAssert.assertThat(
            "a chain without messages must have no log entries",
            new Causes(new RuntimeException(null, new RuntimeException())),
            Matchers.emptyIterable()
        );
    }

    @Test
    void retainsExistingDuplicateFiltering() {
        MatcherAssert.assertThat(
            "redundant messages must still collapse to their underlying reason",
            new Causes(new RuntimeException("wrapped root", new RuntimeException("root"))),
            Matchers.contains("root")
        );
    }
}
