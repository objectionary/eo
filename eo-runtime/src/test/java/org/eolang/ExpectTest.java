/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
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
@SuppressWarnings("PMD.TooManyMethods")
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

    @Test
    void failsWithCorrectTraceForMustAndThat() {
        MatcherAssert.assertThat(
            "Take error message from must",
            Assertions.assertThrows(
                ExFailure.class,
                () -> new Expect<>("attr", () -> "string")
                    .must(i -> false)
                    .otherwise("in must")
                    .that(i -> i)
                    .otherwise("in that")
                    .it(),
                "fails on 'must'"
            ).getMessage(),
            Matchers.equalTo("attr (string) in must")
        );
    }

    @Test
    void failsInTransformingToNumberForNotNumber() {
        MatcherAssert.assertThat(
            "inner class Number working throws error if attr is not a number",
            Assertions.assertThrows(
                ExFailure.class,
                () -> new Expect.Number(
                    Expect.at(
                        new PhWith(
                            new PhDefault(),
                            Attr.RHO,
                            new Data.ToPhi(true)
                        ),
                        Attr.RHO
                    )
                ).it(),
                "fails with correct error message while transform Phi to Number"
            ).getMessage(),
            Matchers.equalTo("the 'ρ' attribute must be a number")
        );
    }

    @Test
    void failsInTransformingToIntegerForNotNumber() {
        MatcherAssert.assertThat(
            "inner class Integer throws error for not a number",
            Assertions.assertThrows(
                ExFailure.class,
                () -> new Expect.Int(
                    Expect.at(
                        new PhWith(
                            new PhDefault(),
                            Attr.RHO,
                            new Data.ToPhi(true)
                        ),
                        Attr.RHO
                    )
                ).it(),
                "fails with correct error message while transform Phi to Integer"
            ).getMessage(),
            Matchers.equalTo("the 'ρ' attribute must be a number")
        );
    }

    @Test
    void failsInTransformingToIntegerForNotInteger() {
        MatcherAssert.assertThat(
            "inner class Integer throws error for not an integer number",
            Assertions.assertThrows(
                ExFailure.class,
                () -> new Expect.Int(
                    Expect.at(
                        new PhWith(
                            new PhDefault(),
                            Attr.RHO,
                            new Data.ToPhi(42.23)
                        ),
                        Attr.RHO
                    )
                ).it(),
                "fails with correct error message while transform Phi to Integer"
            ).getMessage(),
            Matchers.equalTo("the 'ρ' attribute (42.23) must be an integer")
        );
    }

    @Test
    void failsInTransformingToNonNegativeIntegerForNotNumber() {
        MatcherAssert.assertThat(
            "inner class NonNegativeInteger throws error for not a number",
            Assertions.assertThrows(
                ExFailure.class,
                () -> new Expect.Natural(
                    Expect.at(
                        new PhWith(
                            new PhDefault(),
                            Attr.RHO,
                            new Data.ToPhi(true)
                        ),
                        Attr.RHO
                    )
                ).it(),
                "fails with correct error message while transform Phi to NonNegativeInteger"
            ).getMessage(),
            Matchers.equalTo("the 'ρ' attribute must be a number")
        );
    }

    @Test
    void failsInTransformingToNonNegativeIntegerForNotInteger() {
        MatcherAssert.assertThat(
            "inner class NonNegativeInteger throws error for not an integer number",
            Assertions.assertThrows(
                ExFailure.class,
                () -> new Expect.Natural(
                    Expect.at(
                        new PhWith(
                            new PhDefault(),
                            Attr.RHO,
                            new Data.ToPhi(42.23)
                        ),
                        Attr.RHO
                    )
                ).it(),
                "fails with correct error message while transform Phi to NonNegativeInteger"
            ).getMessage(),
            Matchers.equalTo("the 'ρ' attribute (42.23) must be an integer")
        );
    }

    @Test
    void failsInTransformingToNonNegativeIntegerForNegative() {
        MatcherAssert.assertThat(
            "inner class NonNegativeInteger throws error for a negative integer",
            Assertions.assertThrows(
                ExFailure.class,
                () -> new Expect.Natural(
                    Expect.at(
                        new PhWith(
                            new PhDefault(),
                            Attr.RHO,
                            new Data.ToPhi(-42)
                        ),
                        Attr.RHO
                    )
                ).it(),
                "fails with correct error message while transform Phi to NonNegativeInteger"
            ).getMessage(),
            Matchers.equalTo("the 'ρ' attribute (-42) must be greater or equal to zero")
        );
    }

}
