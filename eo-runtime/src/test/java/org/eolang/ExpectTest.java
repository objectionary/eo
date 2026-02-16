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
    void failsWithOneError() {
        Assertions.assertThrows(
            ExFailure.class,
            () -> new Expect<>("a number", () -> 42)
                .must(i -> i < 0)
                .otherwise("must be negative")
                .it(),
            "Expect should fail when must condition is not satisfied, but it didn't"
        );
    }

    @Test
    void failsWithTwoErrors() {
        Assertions.assertThrows(
            ExFailure.class,
            () -> new Expect<>("a number", () -> 42.2)
                .must(i -> i < 0)
                .otherwise("must be negative")
                .must(i -> i % 1 == 0)
                .otherwise("must be an integer")
                .it(),
            "Expect should fail on first unsatisfied condition, but it didn't"
        );
    }

    @Test
    void failsWithOneOkAndOneError() {
        Assertions.assertThrows(
            ExFailure.class,
            () -> new Expect<>("a number", () -> 42.2)
                .must(i -> i > 0)
                .otherwise("must be positive")
                .must(i -> i % 1 == 0)
                .otherwise("must be an integer")
                .it(),
            "Expect should fail on second condition when first passes, but it didn't"
        );
    }

    @Test
    void failsWithExFailureInThat() {
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
            "Expect should fail when that() throws ExFailure, but it didn't"
        );
    }

    @Test
    void failsWithExFailureInThatForParsing() {
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
            "Expect should fail when that() throws parsing error, but it didn't"
        );
    }

    @Test
    void failsForMustBeforeThat() {
        Assertions.assertThrows(
            ExFailure.class,
            () -> new Expect<>("attr", () -> "string")
                .must(i -> false)
                .otherwise("in must")
                .that(i -> i)
                .otherwise("in that")
                .it(),
            "Expect should fail on must() before that(), but it didn't"
        );
    }

    @Test
    void failsInTransformingToNumberForNotNumber() {
        Assertions.assertThrows(
            ExFailure.class,
            () -> new Expect.Number(
                Expect.at(
                    new PhWith(
                        new PhDefault(),
                        Phi.RHO,
                        new Data.ToPhi(true)
                    ),
                    Phi.RHO
                )
            ).it(),
            "Expect.Number should fail when attribute is not a number, but it didn't"
        );
    }

    @Test
    void failsInTransformingToIntegerForNotNumber() {
        Assertions.assertThrows(
            ExFailure.class,
            () -> new Expect.Int(
                Expect.at(
                    new PhWith(
                        new PhDefault(),
                        Phi.RHO,
                        new Data.ToPhi(true)
                    ),
                    Phi.RHO
                )
            ).it(),
            "Expect.Int should fail when attribute is not a number, but it didn't"
        );
    }

    @Test
    void failsInTransformingToIntegerForNotInteger() {
        Assertions.assertThrows(
            ExFailure.class,
            () -> new Expect.Int(
                Expect.at(
                    new PhWith(
                        new PhDefault(),
                        Phi.RHO,
                        new Data.ToPhi(42.23)
                    ),
                    Phi.RHO
                )
            ).it(),
            "Expect.Int should fail when attribute is not an integer, but it didn't"
        );
    }

    @Test
    void failsInTransformingToNaturalForNotNumber() {
        Assertions.assertThrows(
            ExFailure.class,
            () -> new Expect.Natural(
                Expect.at(
                    new PhWith(
                        new PhDefault(),
                        Phi.RHO,
                        new Data.ToPhi(true)
                    ),
                    Phi.RHO
                )
            ).it(),
            "Expect.Natural should fail when attribute is not a number, but it didn't"
        );
    }

    @Test
    void failsInTransformingToNaturalForNotInteger() {
        Assertions.assertThrows(
            ExFailure.class,
            () -> new Expect.Natural(
                Expect.at(
                    new PhWith(
                        new PhDefault(),
                        Phi.RHO,
                        new Data.ToPhi(42.23)
                    ),
                    Phi.RHO
                )
            ).it(),
            "Expect.Natural should fail when attribute is not an integer, but it didn't"
        );
    }

    @Test
    void failsInTransformingToNaturalForNegative() {
        Assertions.assertThrows(
            ExFailure.class,
            () -> new Expect.Natural(
                Expect.at(
                    new PhWith(
                        new PhDefault(),
                        Phi.RHO,
                        new Data.ToPhi(-42)
                    ),
                    Phi.RHO
                )
            ).it(),
            "Expect.Natural should fail when attribute is negative, but it didn't"
        );
    }

}
