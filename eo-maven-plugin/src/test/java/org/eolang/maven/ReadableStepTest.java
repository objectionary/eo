/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

/**
 * Test case for {@link ReadableStep}.
 * @since 0.57.0
 */
final class ReadableStepTest {

    @Test
    void acceptsTheOnlyReadableStep() {
        MatcherAssert.assertThat(
            "a step of 2 is the only one the parser can read back, so it must be accepted",
            new ReadableStep(2).value(),
            Matchers.equalTo(2)
        );
    }

    @ParameterizedTest
    @ValueSource(ints = {4, 0, -2})
    void rejectsUnreadableStep(final int step) {
        MatcherAssert.assertThat(
            "a step other than 2 must be rejected with a clear message naming the parameter",
            Assertions.assertThrows(
                IllegalArgumentException.class,
                () -> new ReadableStep(step).value(),
                "must throw, not silently produce a weight the parser cannot read back"
            ).getMessage(),
            Matchers.containsString("eo.step")
        );
    }
}
