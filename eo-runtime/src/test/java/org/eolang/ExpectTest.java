/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2025 Objectionary.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */
package org.eolang;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test for {@link Expect}.
 *
 * @since 0.1.0
 */
final class ExpectTest {

    @Test
    void buildsAndChecksWithoutErrors() {
        MatcherAssert.assertThat(
            "Passes checks",
            new Expect<>("something", () -> 42)
                .must(i -> i > 0)
                .otherwise("must be positive")
                .that(i -> Integer.toString(i))
                .must(s -> !s.isEmpty())
                .it(),
            Matchers.equalTo("42")
        );
    }

    @Test
    void failsWithCorrectTraceWithOneError() {
        MatcherAssert.assertThat(
            "Throw error in first 'must'. Error message is correct",
            Assertions.assertThrows(
                ExFailure.class,
                () -> new Expect<>("a number", () -> 42)
                    .must(i -> i < 0)
                    .otherwise("must be negative")
                    .it(),
                "fails on check"
            ).getMessage(),
            Matchers.equalTo("a number (42) must be negative")
        );
    }

    @Test
    void failsWithCorrectTraceWithTwoErrors() {
        MatcherAssert.assertThat(
            "Throw error in first 'must'. Not add error about second 'must'",
            Assertions.assertThrows(
                ExFailure.class,
                () -> new Expect<>("a number", () -> 42.2)
                    .must(i -> i < 0)
                    .otherwise("must be negative")
                    .must(i -> i % 1 == 0)
                    .otherwise("must be an integer")
                    .it(),
                "fails only for first 'must'"
            ).getMessage(),
            Matchers.equalTo("a number (42.2) must be negative")
        );
    }

    @Test
    void failsWithCorrectTraceWithOneOkAndOneError() {
        MatcherAssert.assertThat(
            "Throw error in second 'must'. First 'must' passes check",
            Assertions.assertThrows(
                ExFailure.class,
                () -> new Expect<>("a number", () -> 42.2)
                    .must(i -> i > 0)
                    .otherwise("must be positive")
                    .must(i -> i % 1 == 0)
                    .otherwise("must be an integer")
                    .it(),
                "fails on checking integer"
            ).getMessage(),
            Matchers.equalTo("a number (42.2) must be an integer")
        );
    }

    @Test
    void failsWithCorrectTraceWithExFailureInThat() {
        MatcherAssert.assertThat(
            "Take error message from 'otherwise', not from original error",
            Assertions.assertThrows(
                ExFailure.class,
                () -> new Expect<>("attr", () -> 42.2)
                    .that(
                        i -> {
                            throw new ExFailure("Some error in operation");
                        }
                    )
                    .otherwise("must be converted to something")
                    .it(),
                "fails on 'that' because of some internal error"
            ).getMessage(),
            Matchers.equalTo("attr must be converted to something")
        );
    }

    @Test
    void failsWithCorrectTraceWithExFailureInThatForParsing() {
        MatcherAssert.assertThat(
            "Take error message from 'otherwise', not from original error",
            Assertions.assertThrows(
                ExFailure.class,
                () -> new Expect<>("attr", () -> "string")
                    .that(
                        i -> {
                            try {
                                return Integer.parseInt(i);
                            } catch (final NumberFormatException ex) {
                                throw new ExFailure("Can't parse to integer", ex);
                            }
                        }
                    )
                    .otherwise("must be an integer")
                    .it(),
                "fails on 'that' because can not parse"
            ).getMessage(),
            Matchers.equalTo("attr must be an integer")
        );
    }
}
