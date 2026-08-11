/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

/*
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package org.eolang.EO_string; // NOPMD

import org.eolang.BytesOf;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.ExFailure;
import org.eolang.PhApplication;
import org.eolang.Phi;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

/**
 * Test case for {@link EOas_decimal$EOfrom_i64}.
 * @since 0.74.0
 */
final class EOas_decimalEOfrom_i64Test {

    @ParameterizedTest
    @CsvSource({
        "-9223372036854775808, -9223372036854775808",
        "9223372036854775807, 9223372036854775807",
        "0, 0",
        "42, 42",
        "-42, -42",
        "38802277692848472, 38802277692848472"
    })
    void convertsSignedLongToDecimalBytes(final long input, final String expected) {
        MatcherAssert.assertThat(
            "signed i64 bytes must become exact decimal UTF-8 bytes",
            new Dataized(EOas_decimalEOfrom_i64Test.application(input)).asString(),
            Matchers.equalTo(expected)
        );
    }

    @Test
    void rejectsWrongWidthInput() {
        Assertions.assertThrows(
            ExFailure.class,
            () -> new Dataized(
                EOas_decimalEOfrom_i64Test.application(
                    new Data.ToPhi(new byte[]{0x00})
                )
            ).take(),
            "an i64 atom must reject input that is not exactly eight bytes"
        );
    }

    @Test
    void returnsBytes() {
        MatcherAssert.assertThat(
            "the atom result must honor its declared Q.bytes forma",
            EOas_decimalEOfrom_i64Test.application(42L).take(Phi.LAMBDA).forma(),
            Matchers.equalTo("Φ.bytes")
        );
    }

    /**
     * Apply the atom to exact signed-long bytes.
     * @param value Signed long
     * @return Atom application
     */
    private static Phi application(final long value) {
        return EOas_decimalEOfrom_i64Test.application(
            new Data.ToPhi(new BytesOf(value).take())
        );
    }

    /**
     * Apply the atom to the provided object.
     * @param value Value object
     * @return Atom application
     */
    private static Phi application(final Phi value) {
        return new PhApplication(
            new EOas_decimal$EOfrom_i64(), "value", value
        );
    }
}
