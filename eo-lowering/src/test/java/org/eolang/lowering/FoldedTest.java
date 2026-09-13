/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.util.Arrays;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

/**
 * Test case for {@link Folded}.
 *
 * @since 0.77.0
 */
final class FoldedTest {

    @ParameterizedTest
    @CsvSource(
        delimiter = '|',
        value = {
            "L_number_plus|number:40-00-00-00-00-00-00-00|number:40-08-00-00-00-00-00-00|Φ.number( φ ↦ Φ.bytes( φ ↦ ⟦ Δ ⤍ 40-14-00-00-00-00-00-00 ⟧ ) )",
            "L_number_times|number:40-00-00-00-00-00-00-00|number:40-08-00-00-00-00-00-00|Φ.number( φ ↦ Φ.bytes( φ ↦ ⟦ Δ ⤍ 40-18-00-00-00-00-00-00 ⟧ ) )",
            "L_number_div|number:40-18-00-00-00-00-00-00|number:40-00-00-00-00-00-00-00|Φ.number( φ ↦ Φ.bytes( φ ↦ ⟦ Δ ⤍ 40-08-00-00-00-00-00-00 ⟧ ) )",
            "L_number_gt|number:40-08-00-00-00-00-00-00|number:40-00-00-00-00-00-00-00|Φ.true",
            "L_bytes_and|bytes:0F-FF|bytes:F0-|Φ.bytes( φ ↦ ⟦ Δ ⤍ 00-00 ⟧ )",
            "L_bytes_or|bytes:0F-|bytes:F0-|Φ.bytes( φ ↦ ⟦ Δ ⤍ FF- ⟧ )",
            "L_bytes_concat|bytes:01-|bytes:02-03|Φ.bytes( φ ↦ ⟦ Δ ⤍ 01-02-03 ⟧ )",
            "L_bytes_eq|bytes:01-02|bytes:01-03|Φ.false"
        }
    )
    void foldsBinaryOperationOverLiterals(final String lambda, final String left,
        final String right, final String phi) {
        MatcherAssert.assertThat(
            String.format("the %s of two literals must fold to data, but it didnt", lambda),
            new Folded(new Op(lambda), Arrays.asList(left, right)).phi(),
            Matchers.equalTo(phi)
        );
    }

    @Test
    void foldsBytesNot() {
        MatcherAssert.assertThat(
            "the bitwise not of a literal must fold, but it didnt",
            new Folded(new Op("L_bytes_not"), Arrays.asList("bytes:F0-0F")).phi(),
            Matchers.equalTo("Φ.bytes( φ ↦ ⟦ Δ ⤍ 0F-F0 ⟧ )")
        );
    }

    @Test
    void foldsBytesSizeToNumber() {
        MatcherAssert.assertThat(
            "the size of literal bytes must fold to a number, but it didnt",
            new Folded(new Op("L_bytes_size"), Arrays.asList("bytes:01-02-03")).phi(),
            Matchers.equalTo("Φ.number( φ ↦ Φ.bytes( φ ↦ ⟦ Δ ⤍ 40-08-00-00-00-00-00-00 ⟧ ) )")
        );
    }

    @Test
    void leavesUnknownOperationUnfolded() {
        MatcherAssert.assertThat(
            "an operation without a fold must answer blank, but it didnt",
            new Folded(new Op("L_bytes_right"), Arrays.asList("bytes:01-02", "number:40-00-00-00-00-00-00-00")).phi(),
            Matchers.is(Matchers.emptyString())
        );
    }
}
