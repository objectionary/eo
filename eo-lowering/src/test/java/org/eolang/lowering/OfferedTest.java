/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Offered}.
 *
 * @since 0.76.0
 */
final class OfferedTest {

    @Test
    void answersOperationOfTheTable() {
        MatcherAssert.assertThat(
            "the universe must answer a method the table binds to the forma, but it doesnt",
            new Offered("number").has("plus"),
            Matchers.is(true)
        );
    }

    @Test
    void answersBytesOperationForString() {
        MatcherAssert.assertThat(
            "a string decorates its bytes, so it must answer a bytes operation, but it doesnt",
            new Offered("string").has("size"),
            Matchers.is(true)
        );
    }

    @Test
    void refusesMethodOfEo() {
        MatcherAssert.assertThat(
            "a method the table lacks is not answered, but it is",
            new Offered("number").has("minus"),
            Matchers.is(false)
        );
    }

    @Test
    void refusesNumberOperationForBytes() {
        MatcherAssert.assertThat(
            "bytes do not decorate a number, so they must not answer its operations, but they do",
            new Offered("bytes").has("plus"),
            Matchers.is(false)
        );
    }

    @Test
    void refusesAnythingForObject() {
        MatcherAssert.assertThat(
            "an object answers only its dataization, but it answered more",
            new Offered("object").has("length"),
            Matchers.is(false)
        );
    }
}
