/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Op}.
 * @since 0.76.0
 */
final class OpTest {

    @Test
    void findsMethodOfLambda() {
        MatcherAssert.assertThat(
            "the λ of addition must dispatch as plus, but it doesnt",
            new Op("L_number_plus").method(),
            Matchers.equalTo("plus")
        );
    }

    @Test
    void namesFormaOfComparison() {
        MatcherAssert.assertThat(
            "a comparison must answer a bool, but this one doesnt",
            new Op("L_number_gt").forma(),
            Matchers.equalTo("bool")
        );
    }

    @Test
    void namesCarrierOfBytesOperation() {
        MatcherAssert.assertThat(
            "the size of bytes must dispatch on a bytes receiver, but it doesnt",
            new Op("L_bytes_size").carrier(),
            Matchers.equalTo("bytes")
        );
    }

    @Test
    void listsArgumentsOfSlice() {
        MatcherAssert.assertThat(
            "slicing must take its bounds in positional order, but it doesnt",
            new Op("L_bytes_slice").args(),
            Matchers.contains("start", "len")
        );
    }

    @Test
    void namesFormaOfIndexArgument() {
        MatcherAssert.assertThat(
            "the index of a tuple must carry a number where the tuple carries none, but it doesnt",
            new Op("L_tuple_at").formas(),
            Matchers.contains("number")
        );
    }

    @Test
    void namesArgumentWithoutItsForma() {
        MatcherAssert.assertThat(
            "the forma of an argument must not leak into its name, but it did",
            new Op("L_tuple_at").args(),
            Matchers.contains("i")
        );
    }

    @Test
    void carriesArgumentsAsTheReceiverByDefault() {
        MatcherAssert.assertThat(
            "an argument without a forma of its own must carry the receiver's, but it doesnt",
            new Op("L_number_plus").formas(),
            Matchers.contains("number")
        );
    }

    @Test
    void namesNoFormaOfChoice() {
        MatcherAssert.assertThat(
            "a choice answers whatever its arms answer, so it must name no forma, but it does",
            new Op("L_bool_if").forma(),
            Matchers.emptyString()
        );
    }

    @Test
    void listsArmsOfChoice() {
        MatcherAssert.assertThat(
            "a choice must take its two arms in positional order, but it doesnt",
            new Op("L_bool_if").args(),
            Matchers.contains("t", "f")
        );
    }

    @Test
    void listsNoArgumentsOfSize() {
        MatcherAssert.assertThat(
            "the size of bytes must take no arguments, but it does",
            new Op("L_bytes_size").args(),
            Matchers.empty()
        );
    }

    @Test
    void rendersAdditionAsJava() {
        MatcherAssert.assertThat(
            "the addition must render as the plus operator, but it doesnt",
            String.format(new Op("L_number_plus").java(), "a", "b"),
            Matchers.equalTo("a + b")
        );
    }

    @Test
    void refusesRenderingOfShift() {
        Assertions.assertThrows(
            IllegalStateException.class,
            new Op("L_bytes_right")::java,
            "the shift has no faithful Java rendering, but one was given"
        );
    }

    @Test
    void disownsUnknownLambda() {
        MatcherAssert.assertThat(
            "an atom outside the table cannot be listed, but it is",
            new Op("L_number_minus").listed(),
            Matchers.is(false)
        );
    }

    @Test
    void refusesUnknownLambda() {
        Assertions.assertThrows(
            IllegalStateException.class,
            new Op("L_string_length")::method,
            "an atom outside the table cannot name a method, but it did"
        );
    }
}
