/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link PhDefault} data isolation.
 * @since 0.1
 */
final class PhDefaultDataTest {

    @Test
    void isolatesBytesPassedToConstructor() {
        final byte[] bytes = {0x01};
        final Phi object = new PhDefault(bytes);
        bytes[0] = 0x02;
        MatcherAssert.assertThat(
            "PhDefault snapshot must not be altered: " + object.φTerm(),
            object.delta(),
            Matchers.equalTo(new byte[] {0x01})
        );
    }

    @Test
    void doesNotExposeItsBytes() {
        final Phi object = new PhDefault(new byte[] {0x01});
        final byte[] exposed = object.delta();
        exposed[0] = 0x02;
        MatcherAssert.assertThat(
            "PhDefault must not allow delta bytes to mutate its data",
            object.delta(),
            Matchers.equalTo(new byte[] {0x01})
        );
    }
}
