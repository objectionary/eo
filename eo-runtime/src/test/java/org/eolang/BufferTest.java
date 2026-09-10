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
 * Test case for {@link Buffer}.
 *
 * @since 0.64.0
 */
final class BufferTest {

    @Test
    void refusesSizeTheHeapCannotHold() {
        MatcherAssert.assertThat(
            "a size larger than any array must be refused by name, but it wasnt",
            Assertions.assertThrows(
                ExFailure.class,
                () -> new Buffer("the 'size' argument of read", Integer.MAX_VALUE).it(),
                "a size larger than any array was expected to fail with ExFailure"
            ).getMessage(),
            Matchers.allOf(
                Matchers.containsString("'size' argument of read"),
                Matchers.containsString("Can't allocate")
            )
        );
    }

    @Test
    void makesArrayOfTheSizeAsked() {
        MatcherAssert.assertThat(
            "the array must be as long as the size asked for, but it wasnt",
            new Buffer("the 'size' argument of recv", 16).it().length,
            Matchers.equalTo(16)
        );
    }
}
