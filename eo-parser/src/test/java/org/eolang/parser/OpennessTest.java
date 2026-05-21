/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Openness}.
 * @since 0.1
 */
final class OpennessTest {

    @Test
    void enumeratesThreeStates() {
        MatcherAssert.assertThat(
            "openness must offer exactly the three §1.6 states",
            Openness.values().length,
            Matchers.equalTo(3)
        );
    }

    @Test
    void offersOpenState() {
        MatcherAssert.assertThat(
            "OPEN must be one of the openness states",
            Openness.valueOf("OPEN"),
            Matchers.notNullValue()
        );
    }

    @Test
    void offersVerticalCompletedState() {
        MatcherAssert.assertThat(
            "VERTICAL_COMPLETED must be one of the openness states",
            Openness.valueOf("VERTICAL_COMPLETED"),
            Matchers.notNullValue()
        );
    }

    @Test
    void offersHorizontalCompletedState() {
        MatcherAssert.assertThat(
            "HORIZONTAL_COMPLETED must be one of the openness states",
            Openness.valueOf("HORIZONTAL_COMPLETED"),
            Matchers.notNullValue()
        );
    }
}
