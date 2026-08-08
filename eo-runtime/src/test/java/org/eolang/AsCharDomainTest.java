/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import org.eolang.EO_string.EOas_char;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

/**
 * Test case for the domain validation of {@code string.as-char}.
 * @since 0.57
 */
@SuppressWarnings("JTCOP.RuleAllTestsHaveProductionClass")
final class AsCharDomainTest {

    @ParameterizedTest
    @ValueSource(doubles = {-1.0, 256.0, 300.0, 3.5})
    void rejectsCodeOutsideTheByteRange(final double code) {
        Assertions.assertThrows(
            ExAbstract.class,
            () -> new Dataized(AsCharDomainTest.asChar(code)).take(),
            "as-char must reject a code outside the [0, 255] integer domain"
        );
    }

    @ParameterizedTest
    @ValueSource(doubles = {0.0, 65.0, 255.0})
    void acceptsCodeInsideTheByteRange(final double code) {
        MatcherAssert.assertThat(
            "as-char must accept a valid byte code and emit a single byte",
            new Dataized(AsCharDomainTest.asChar(code)).take().length,
            Matchers.equalTo(1)
        );
    }

    /**
     * Build {@code as-char} applied to the given code.
     * @param code The numeric code
     * @return The as-char object
     */
    private static Phi asChar(final double code) {
        final Phi phi = new EOas_char().copy();
        phi.put(0, new Data.ToPhi(code));
        return phi;
    }
}
