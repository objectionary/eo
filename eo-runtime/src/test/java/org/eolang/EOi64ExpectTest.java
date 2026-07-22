/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

/*
 * @checkstyle PackageNameCheck (10 lines)
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package org.eolang;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

/**
 * Test case verifying {@link Expect}-based error messages
 * raised by the {@code EOi64$*} arithmetic atoms.
 * @since 0.51
 * @checkstyle TypeNameCheck (4 lines)
 */
@SuppressWarnings("JTCOP.RuleAllTestsHaveProductionClass")
final class EOi64ExpectTest {

    @ParameterizedTest
    @ValueSource(classes = EOi64$EOdiv.class)
    void throwsCorrectErrorForXAttr(final Class<?> cls) {
        MatcherAssert.assertThat(
            "the message in the error is correct",
            Assertions.assertThrows(
                ExAbstract.class,
                () -> new Dataized(
                    new PhApplication(
                        new PhApplication(
                            (Phi) cls.getDeclaredConstructor().newInstance(),
                            Phi.RHO,
                            new Data.ToPhi(42L)
                        ),
                        "x",
                        new Data.ToPhi(true)
                    )
                ).take(),
                "operation with TRUE fails with a proper message that explains what happened"
            ).getMessage(),
            Matchers.equalTo("the 'x' attribute must be an i64")
        );
    }

    @ParameterizedTest
    @ValueSource(classes = EOi64$EOdiv.class)
    void throwsCorrectErrorForRhoAttr(final Class<?> cls) {
        MatcherAssert.assertThat(
            "the message in the error is correct",
            Assertions.assertThrows(
                ExAbstract.class,
                () -> new Dataized(
                    new PhApplication(
                        (Phi) cls.getDeclaredConstructor().newInstance(),
                        Phi.RHO,
                        new Data.ToPhi(true)
                    )
                ).take(),
                "EOi64 must be an i64"
            ).getMessage(),
            Matchers.equalTo("the 'ρ' attribute must be an i64")
        );
    }
}
