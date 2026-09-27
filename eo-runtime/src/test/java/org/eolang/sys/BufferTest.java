/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.sys;

import org.eolang.ExFailure;
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
    void makesAnArrayOfTheAskedSize() {
        MatcherAssert.assertThat(
            "A size the heap can hold must come back as an array of exactly that many bytes",
            new Buffer("the 'size' argument of read", 17).it().length,
            Matchers.equalTo(17)
        );
    }

    @Test
    void makesNothingOutOfNothing() {
        MatcherAssert.assertThat(
            "A size of zero must come back as an empty array, not as a failure",
            new Buffer("the 'size' argument of recv", 0).it().length,
            Matchers.equalTo(0)
        );
    }

    @Test
    void refusesASizeTheHeapCannotHold() {
        Assertions.assertThrows(
            ExFailure.class,
            () -> new Buffer("the 'size' argument of read", Integer.MAX_VALUE).it(),
            "A size larger than the heap must fail with ExFailure, not with OutOfMemoryError"
        );
    }

    @Test
    void namesWhatTheSizeWasFor() {
        MatcherAssert.assertThat(
            "The failure must name the argument it refused, so the message says what to fix",
            Assertions.assertThrows(
                ExFailure.class,
                () -> new Buffer("the 'size' argument of recv", Integer.MAX_VALUE).it()
            ).getMessage(),
            Matchers.containsString("the 'size' argument of recv")
        );
    }
}
