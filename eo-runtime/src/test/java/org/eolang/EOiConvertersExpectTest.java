/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

/*
 * @checkstyle PackageNameCheck (10 lines)
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package org.eolang;

import java.util.stream.Stream;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

/**
 * Test case verifying {@link Expect}-based error messages
 * raised by the {@code EOi16/i32/i64} converter atoms.
 * @since 0.51
 * @checkstyle TypeNameCheck (4 lines)
 */
@SuppressWarnings("JTCOP.RuleAllTestsHaveProductionClass")
final class EOiConvertersExpectTest {

    @ParameterizedTest
    @MethodSource("converters")
    void throwsCorrectErrorForRhoAttr(final Class<?> cls, final String expected) {
        MatcherAssert.assertThat(
            "the message in the error is correct",
            Assertions.assertThrows(
                ExAbstract.class,
                () -> new Dataized(
                    new PhWith(
                        (Phi) cls.getDeclaredConstructor().newInstance(),
                        Phi.RHO,
                        new Data.ToPhi(true)
                    )
                ).take(),
                "conversion with a non-integer rho must fail with a proper message"
            ).getMessage(),
            Matchers.equalTo(expected)
        );
    }

    private static Stream<Arguments> converters() {
        return Stream.of(
            Arguments.of(EOi16$EOas_i32.class, "the 'ρ' attribute must be an i16"),
            Arguments.of(EOi32$EOas_i64.class, "the 'ρ' attribute must be an i32"),
            Arguments.of(EOi64$EOas_number.class, "the 'ρ' attribute must be an i64")
        );
    }
}
