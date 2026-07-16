/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test cases for {@link Diff}.
 * @since 0.57.0
 */
final class DiffTest {

    @Test
    void detectsIdenticalTexts() {
        MatcherAssert.assertThat(
            "identical texts must be reported as the same",
            new Diff(
                String.format("a%nb%nc"), String.format("a%nb%nc")
            ).same(),
            Matchers.is(true)
        );
    }

    @Test
    void rendersNothingForIdenticalTexts() {
        MatcherAssert.assertThat(
            "identical texts must produce an empty diff",
            new Diff(String.format("a%nb"), String.format("a%nb")).colored(),
            Matchers.isEmptyString()
        );
    }

    @Test
    void detectsDifferentTexts() {
        MatcherAssert.assertThat(
            "different texts must not be reported as the same",
            new Diff(String.format("a%nb"), String.format("a%nc")).same(),
            Matchers.is(false)
        );
    }

    @Test
    void highlightsRemovedAndAddedLines() {
        MatcherAssert.assertThat(
            "the diff must keep the common line and color the changed ones",
            new Diff(String.format("a%nb%nc"), String.format("a%nB%nc")).colored(),
            Matchers.stringContainsInOrder(" a", "[31m-b", "[32m+B", " c")
        );
    }

    @Test
    void highlightsTrailingAdditions() {
        MatcherAssert.assertThat(
            "extra trailing lines must be shown as additions",
            new Diff("a", String.format("a%nb")).colored(),
            Matchers.containsString("[32m+b")
        );
    }
}
