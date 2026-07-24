/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Taken}.
 * @since 0.60
 */
final class TakenTest {

    @Test
    void keepsTheObjectItWasBuiltWith() {
        final Phi object = new Data.ToPhi(42L);
        MatcherAssert.assertThat(
            "Taken must expose the very object it was given, but it does not",
            new Taken(object, true).phi(),
            Matchers.sameInstance(object)
        );
    }

    @Test
    void reportsTheImpureFlag() {
        MatcherAssert.assertThat(
            "Taken must report the purity flag it was given, but it does not",
            new Taken(new Data.ToPhi(-7L), false).pure(),
            Matchers.is(false)
        );
    }
}
