/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
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
            "Hash codes of two Data.ToPhi instances with the same value should differ, but they didn't",
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
            "Data.ToPhi instances with the same long value should differ, but they didn't",
            new Data.ToPhi(1L),
            Matchers.not(Matchers.equalTo(new Data.ToPhi(1L)))
        );
        MatcherAssert.assertThat(
            "Data.ToPhi instances with the same string value should differ, but they didn't",
            new Data.ToPhi("Welcome"),
            Matchers.not(Matchers.equalTo(new Data.ToPhi("Welcome")))
        );
        MatcherAssert.assertThat(
            "Data.ToPhi instances with the same double value should differ, but they didn't",
            new Data.ToPhi(2.18d),
            Matchers.not(Matchers.equalTo(new Data.ToPhi(2.18d)))
        );
        MatcherAssert.assertThat(
            "Data.ToPhi instances with the same byte array value should differ, but they didn't",
            new Data.ToPhi(new byte[] {(byte) 0x00, (byte) 0x1f}),
            Matchers.not(Matchers.equalTo(new Data.ToPhi(new byte[] {(byte) 0x00, (byte) 0x1f})))
        );
    }
}
