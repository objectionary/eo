/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for the data-immutability of {@link PhDefault}.
 * @since 0.1
 */
final class PhDefaultDataTest {

    @Test
    void protectsDataFromEscapedArrayMutation() {
        final byte[] raw = {(byte) 0x01};
        final Phi phi = new PhDefault(raw);
        raw[0] = (byte) 0x02;
        phi.delta()[0] = (byte) 0x03;
        MatcherAssert.assertThat(
            "PhDefault must snapshot its data and never leak a mutable array, but it did",
            phi.delta(),
            Matchers.equalTo(new byte[] {(byte) 0x01})
        );
    }
}
