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
 * Test case for {@link Lru}.
 * @since 0.75
 */
final class LruTest {

    @Test
    void keepsNothingWhenCapacityIsZero() {
        final Lru<byte[]> map = new Lru<>(0);
        map.put("a", new byte[] {(byte) 0x01});
        MatcherAssert.assertThat(
            "a map of no capacity must not remember what was put into it, but it did",
            map.get("a"),
            Matchers.nullValue()
        );
    }

    @Test
    void staysEmptyWhenCapacityIsZero() {
        final Lru<byte[]> map = new Lru<>(0);
        map.put("a", new byte[] {(byte) 0x01});
        map.put("b", new byte[] {(byte) 0x02});
        MatcherAssert.assertThat(
            "a map of no capacity must stay empty, but it didnt",
            map.size(),
            Matchers.equalTo(0)
        );
    }

    @Test
    void refusesNegativeCapacity() {
        Assertions.assertThrows(
            IllegalArgumentException.class,
            () -> new Lru<byte[]>(-1),
            "a negative capacity must be refused by the constructor, but it wasnt"
        );
    }
}
