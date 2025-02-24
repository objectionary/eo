/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Data}.
 *
 * @since 0.1
 */
final class DataTest {
    @Test
    void comparesVertex() {
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
            new Data.ToPhi(42L).hashCode(),
            Matchers.not(
                Matchers.equalTo(
                    new Data.ToPhi(42L).hashCode()
                )
            )
        );
    }

    @Test
    void comparesTwoDatas() {
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
            new Data.ToPhi(1L),
            Matchers.not(Matchers.equalTo(new Data.ToPhi(1L)))
        );
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
            new Data.ToPhi("Welcome"),
            Matchers.not(Matchers.equalTo(new Data.ToPhi("Welcome")))
        );
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
            new Data.ToPhi(2.18d),
            Matchers.not(Matchers.equalTo(new Data.ToPhi(2.18d)))
        );
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
            new Data.ToPhi(new byte[] {(byte) 0x00, (byte) 0x1f}),
            Matchers.not(Matchers.equalTo(new Data.ToPhi(new byte[] {(byte) 0x00, (byte) 0x1f})))
        );
    }
}
