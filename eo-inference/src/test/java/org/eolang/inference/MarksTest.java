/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Marks}.
 *
 * @since 0.76.0
 */
final class MarksTest {

    @Test
    void givesZeroForValueNothingWasNotedOf() {
        MatcherAssert.assertThat(
            "a value nothing was noted of must give zero, but it didnt",
            new Marks().get(70_000),
            Matchers.equalTo(0)
        );
    }

    @Test
    void givesBackWhatWasNotedFarAhead() {
        final Marks marks = new Marks();
        marks.put(12_345, 6);
        MatcherAssert.assertThat(
            "a number noted of a value far past the first ones must come back, but it didnt",
            marks.get(12_345),
            Matchers.equalTo(6)
        );
    }
}
