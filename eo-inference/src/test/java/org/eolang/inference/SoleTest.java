/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.Arrays;
import java.util.Collections;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Sole}.
 * @since 0.71.0
 */
final class SoleTest {

    @Test
    void namesTheOneThingEveryCallerPutsIn() {
        MatcherAssert.assertThat(
            "a void every caller hands an oak must be the oak, but it named something else",
            new Sole(
                Collections.singletonList(new Ref("Φ.oak")),
                Collections.singletonList("Φ.oak")
            ).names(),
            Matchers.equalTo("Φ.oak")
        );
    }

    @Test
    void namesNothingWhereCallersDisagree() {
        MatcherAssert.assertThat(
            "a void handed an oak and an elm cannot be either of them, but one was named",
            new Sole(
                Arrays.asList(new Ref("Φ.oak"), new Ref("Φ.elm")),
                Arrays.asList("Φ.oak", "Φ.elm")
            ).names(),
            Matchers.emptyString()
        );
    }

    @Test
    void namesNothingWhereNobodyPutsAnythingIn() {
        MatcherAssert.assertThat(
            "a void nobody ever fills cannot be anything, but something was named",
            new Sole(Collections.emptyList(), Collections.singletonList("Φ.oak")).names(),
            Matchers.emptyString()
        );
    }

    @Test
    void namesNothingWhereTheTableHasNoSuchRow() {
        MatcherAssert.assertThat(
            "a locator with no row to read cannot be an answer, but it was named",
            new Sole(
                Collections.singletonList(new Ref("Φ.grove.oak")),
                Collections.singletonList("Φ.oak")
            ).names(),
            Matchers.emptyString()
        );
    }

    @Test
    void namesNothingWhereTheOneFillingIsALiteral() {
        MatcherAssert.assertThat(
            "a void handed bytes has no row to read them off, but something was named",
            new Sole(
                Collections.singletonList(new Data()), Collections.singletonList("Φ.oak")
            ).names(),
            Matchers.emptyString()
        );
    }
}
