/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

/*
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package org.eolang.EO_string; // NOPMD

import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.ExAbstract;
import org.eolang.PhApplication;
import org.eolang.Phi;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

/**
 * Test case for {@link EOprintf} diagnostics.
 *
 * @since 0.75.0
 */
@SuppressWarnings("JTCOP.RuleAllTestsHaveProductionClass")
final class EOprintfDiagnosticTest {

    @Test
    void reportsAStringAsNotANumber() {
        MatcherAssert.assertThat(
            "printf must identify a short datum as not a number",
            EOprintfDiagnosticTest.failure(new Data.ToPhi("x")),
            Matchers.allOf(
                Matchers.endsWith(EOprintfDiagnosticTest.notNumber("78")),
                Matchers.not(Matchers.containsString("non-finite"))
            )
        );
    }

    @Test
    void reportsAnEmptyDatumAsNotANumber() {
        MatcherAssert.assertThat(
            "printf must identify an empty datum as not a number",
            EOprintfDiagnosticTest.failure(new Data.ToPhi("")),
            Matchers.endsWith(EOprintfDiagnosticTest.notNumber(""))
        );
    }

    @Test
    void reportsANineByteDatumAsNotANumber() {
        MatcherAssert.assertThat(
            "printf must identify a nine-byte datum as not a number",
            EOprintfDiagnosticTest.failure(new Data.ToPhi("xxxxxxxxx")),
            Matchers.endsWith(
                EOprintfDiagnosticTest.notNumber(
                    "78-78-78-78-78-78-78-78-78"
                )
            )
        );
    }

    @Test
    void keepsFormattingAFiniteNumber() {
        MatcherAssert.assertThat(
            "printf must keep formatting an eight-byte finite number",
            new Dataized(
                EOprintfDiagnosticTest.formatted(new Data.ToPhi(1.25))
            ).take(String.class),
            Matchers.equalTo("1.250000")
        );
    }

    @ParameterizedTest
    @ValueSource(
        doubles = {
            Double.NaN,
            Double.POSITIVE_INFINITY,
            Double.NEGATIVE_INFINITY
        }
    )
    void keepsTheNonFiniteReason(final double number) {
        MatcherAssert.assertThat(
            "printf must preserve the reason for a non-finite number",
            EOprintfDiagnosticTest.failure(new Data.ToPhi(number)),
            Matchers.endsWith(
                "Can't write a non-finite number as a fixed-point decimal"
            )
        );
    }

    private static Phi formatted(final Phi argument) {
        return new PhApplication(
            new Data.ToPhi("%f").take("printf").copy(),
            "args", new Data.ToPhi(new Phi[]{argument})
        );
    }

    private static String failure(final Phi argument) {
        return Assertions.assertThrows(
            ExAbstract.class,
            () -> new Dataized(
                EOprintfDiagnosticTest.formatted(argument)
            ).take()
        ).getMessage();
    }

    private static String notNumber(final String bytes) {
        return String.format(
            "The argument %s is not a number, the %%f conversion takes an eight-byte number",
            bytes
        );
    }
}
