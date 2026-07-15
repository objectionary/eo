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
 * Test case for {@link PhNest}.
 * @since 0.62
 */
final class PhNestTest {

    @Test
    void rejectsDirectPutByPosition() {
        MatcherAssert.assertThat(
            "Direct put by position into a package object must fail fast, but it didn't",
            Assertions.assertThrows(
                ExFailure.class,
                () -> Phi.Φ.take("number").put(0, new Data.ToPhi(42L)),
                "Putting by position straight into a shared package object must be rejected"
            ).getMessage(),
            Matchers.containsString("make a copy first")
        );
    }

    @Test
    void rejectsDirectPutByName() {
        MatcherAssert.assertThat(
            "Direct put by name into a package object must fail fast, but it didn't",
            Assertions.assertThrows(
                ExFailure.class,
                () -> Phi.Φ.take("number").put("x", new Data.ToPhi(42L)),
                "Putting by name straight into a shared package object must be rejected"
            ).getMessage(),
            Matchers.containsString("make a copy first")
        );
    }

    @Test
    void allowsPutAfterCopy() {
        Assertions.assertDoesNotThrow(
            () -> Phi.Φ.take("number").copy().put(0, new Data.ToPhi(42L)),
            "A copy of a package object must accept a put, but it didn't"
        );
    }
}
