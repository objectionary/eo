/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

/*
 * @checkstyle PackageNameCheck (4 lines)
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package org.eolang.EO_string; // NOPMD

import org.cactoos.list.ListOf;
import org.eolang.Data;
import org.eolang.ExFailure;
import org.eolang.Phi;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Tests for {@link PrintfArgs}.
 * @since 0.57.4
 */
@SuppressWarnings("PMD.AvoidDuplicateLiterals")
final class PrintfArgsTest {

    @Test
    void returnsArgumentsForNumberedSubstitution() {
        final Phi tuple = Phi.Φ.take("tuple").copy();
        tuple.put("length", new Data.ToPhi(1));
        final String expected = "Jeff";
        tuple.put("head", new Data.ToPhi(expected));
        MatcherAssert.assertThat(
            "The printf args do not match with expected",
            new PrintfArgs("Hello, %s! Bye, %1$s!", 1L, tuple.take("at")).formatted(),
            Matchers.equalTo(new ListOf<>(expected, expected))
        );
    }

    @Test
    void returnsCorrectArgumentsForMixedSubstitutions() {
        final Phi tuple = Phi.Φ.take("tuple").copy();
        tuple.put("length", new Data.ToPhi(2));
        final Phi root = Phi.Φ.take("tuple").copy();
        root.put("length", new Data.ToPhi(1));
        final String first = "first";
        root.put("head", new Data.ToPhi(first));
        final String second = "second";
        tuple.put("head", new Data.ToPhi(second));
        tuple.put("tail", root);
        MatcherAssert.assertThat(
            "The printf args do not match with expected",
            new PrintfArgs(
                "This is the %s! This is %1$s as well! This is the %s",
                3L,
                tuple.take("at")
            ).formatted(),
            Matchers.equalTo(new ListOf<>(first, first, second))
        );
    }

    @Test
    void reportsOutOfLongRangeMessageInsteadOfDoubleFormatting() {
        final Phi tuple = Phi.Φ.take("tuple").copy();
        tuple.put("length", new Data.ToPhi(1));
        tuple.put("head", new Data.ToPhi(1.0e30));
        MatcherAssert.assertThat(
            "the ExFailure message must name the out-of-range number, not be swallowed by a second, accidental format pass",
            Assertions.assertThrows(
                ExFailure.class,
                () -> new PrintfArgs("%d", 1L, tuple.take("at")).formatted(),
                "must throw ExFailure, not an internal java.util.Formatter exception"
            ).getMessage(),
            Matchers.containsString("doesn't fit into long range")
        );
    }

    @Test
    void rejectsTwoPowerSixtyThreeInsteadOfSaturating() {
        final Phi tuple = Phi.Φ.take("tuple").copy();
        tuple.put("length", new Data.ToPhi(1));
        tuple.put("head", new Data.ToPhi(9_223_372_036_854_775_808.0));
        MatcherAssert.assertThat(
            "the ExFailure message must name the out-of-range number",
            Assertions.assertThrows(
                ExFailure.class,
                () -> new PrintfArgs("%d", 1L, tuple.take("at")).formatted(),
                "2^63 must be rejected, not silently saturated to Long.MAX_VALUE (since (double) Long.MAX_VALUE rounds up to exactly 2^63)"
            ).getMessage(),
            Matchers.containsString("doesn't fit into long range")
        );
    }

    @Test
    void acceptsLargestDoubleThatTrulyFitsInLongRange() {
        final double closest = 9_223_372_036_854_773_760.0;
        final Phi tuple = Phi.Φ.take("tuple").copy();
        tuple.put("length", new Data.ToPhi(1));
        tuple.put("head", new Data.ToPhi(closest));
        MatcherAssert.assertThat(
            "a double strictly below 2^63 (the representable double one ulp below it, since Long.MAX_VALUE itself isn't exactly representable and rounds up to 2^63) must still be accepted as a valid long",
            new PrintfArgs("%d", 1L, tuple.take("at")).formatted(),
            Matchers.equalTo(new ListOf<>((long) closest))
        );
    }

    @Test
    void reportsUnsupportedFormatMessageInsteadOfDoubleFormatting() {
        final Phi tuple = Phi.Φ.take("tuple").copy();
        tuple.put("length", new Data.ToPhi(1));
        tuple.put("head", new Data.ToPhi("x"));
        MatcherAssert.assertThat(
            "the ExFailure message must name the unsupported format char, not be swallowed by a second, accidental format pass",
            Assertions.assertThrows(
                ExFailure.class,
                () -> new PrintfArgs("%z", 1L, tuple.take("at")).formatted(),
                "must throw ExFailure, not an internal java.util.Formatter exception"
            ).getMessage(),
            Matchers.containsString("is unsupported")
        );
    }

    @Test
    void collectsArgumentForWidthSpecifier() {
        final Phi tuple = Phi.Φ.take("tuple").copy();
        tuple.put("length", new Data.ToPhi(1));
        tuple.put("head", new Data.ToPhi(42.0));
        MatcherAssert.assertThat(
            "a specifier with an explicit width, like %5d, must still collect its argument",
            new PrintfArgs("%5d", 1L, tuple.take("at")).formatted(),
            Matchers.equalTo(new ListOf<>(42L))
        );
    }

    @Test
    void collectsArgumentForPrecisionSpecifier() {
        final Phi tuple = Phi.Φ.take("tuple").copy();
        tuple.put("length", new Data.ToPhi(1));
        tuple.put("head", new Data.ToPhi(3.14159));
        MatcherAssert.assertThat(
            "a specifier with a precision, like %.2f, must still collect its argument",
            new PrintfArgs("%.2f", 1L, tuple.take("at")).formatted(),
            Matchers.equalTo(new ListOf<>(3.14159))
        );
    }

    @Test
    void collectsArgumentForFlaggedAndWidthSpecifier() {
        final Phi tuple = Phi.Φ.take("tuple").copy();
        tuple.put("length", new Data.ToPhi(1));
        tuple.put("head", new Data.ToPhi("x"));
        MatcherAssert.assertThat(
            "a specifier with a flag and width, like %-10s, must still collect its argument",
            new PrintfArgs("%-10s", 1L, tuple.take("at")).formatted(),
            Matchers.equalTo(new ListOf<>("x"))
        );
    }
}
