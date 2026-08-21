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
 * Test case for the attribute order of {@link PhDefault}.
 * @since 0.73.4
 */
final class PhDefaultOrderTest {

    @Test
    void doesNotDuplicateOrderWhenTheSameAttributeIsAddedTwice() {
        final PhDefault dup = new PhDefault();
        dup.add("x", new AtVoid("x"));
        dup.add("x", new AtVoid("x"));
        MatcherAssert.assertThat(
            "a positional put past the only attribute must be rejected",
            Assertions.assertThrows(
                ExFailure.class,
                () -> dup.put(1, new Data.ToPhi(5.0)),
                "put past the only attribute must throw"
            ).getMessage(),
            Matchers.containsString("has just 1 attribute")
        );
    }
}
