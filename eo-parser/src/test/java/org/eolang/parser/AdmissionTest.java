/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Admission}.
 *
 * @since 0.1
 */
final class AdmissionTest {

    @Test
    void namesTheLevelWhenLabelIsGiven() {
        final Level level = new Stack().push(0, 1, Kind.HEAD, Openness.OPEN);
        new Admission("nu", false).name(level);
        MatcherAssert.assertThat(
            "a level named through a non-null label must be recorded as named",
            level.named(),
            Matchers.is(true)
        );
    }

    @Test
    void leavesTheLevelUnnamedWhenLabelIsNull() {
        final Level level = new Stack().push(0, 1, Kind.HEAD, Openness.OPEN);
        new Admission(null, false).name(level);
        MatcherAssert.assertThat(
            "a level left untouched by a null label must stay unnamed",
            level.named(),
            Matchers.is(false)
        );
    }
}
