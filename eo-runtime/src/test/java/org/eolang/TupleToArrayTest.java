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
 * Test case for {@link TupleToArray}.
 *
 * @since 0.57
 */
final class TupleToArrayTest {

    @Test
    void rejectsNegativeLengthWithFailure() {
        MatcherAssert.assertThat(
            "a negative tuple length must raise an EO failure, not a Java array exception",
            Assertions.assertThrows(
                ExAbstract.class,
                () -> new TupleToArray(TupleToArrayTest.withLength(-1.0)).get(),
                "a tuple whose length is -1 must fail with a proper message"
            ).getMessage(),
            Matchers.containsString("finite non-negative integer")
        );
    }

    @Test
    void rejectsNonFiniteLengthWithFailure() {
        Assertions.assertThrows(
            ExAbstract.class,
            () -> new TupleToArray(TupleToArrayTest.withLength(Double.POSITIVE_INFINITY)).get(),
            "an infinite tuple length must fail instead of exhausting memory"
        );
    }

    @Test
    void rejectsFractionalLengthWithFailure() {
        Assertions.assertThrows(
            ExAbstract.class,
            () -> new TupleToArray(TupleToArrayTest.withLength(2.7)).get(),
            "a fractional tuple length must fail rather than being silently truncated"
        );
    }

    private static Phi withLength(final double length) {
        final Phi tuple = new PhDefault(
            new Attrs(new Attr("length", new AtVoid("length")))
        ).copy();
        tuple.put("length", new Data.ToPhi(length));
        return tuple;
    }
}
