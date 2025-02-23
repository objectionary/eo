/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

/*
 * @checkstyle PackageNameCheck (10 lines)
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package EOorg.EOeolang; // NOPMD

import org.eolang.AtCompositeTest;
import org.eolang.Attr;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.ExAbstract;
import org.eolang.PhWith;
import org.eolang.Phi;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

/**
 * Test case for {@link EOnumber}.
 *
 * @since 0.39
 * @checkstyle TypeNameCheck (4 lines)
 */
@SuppressWarnings("JTCOP.RuleAllTestsHaveProductionClass")
final class EOnumberTest {

    @Test
    void hasDifferentHashes() {
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
            new Data.ToPhi(42L).hashCode(),
            Matchers.not(Matchers.equalTo(new Data.ToPhi(42L).hashCode()))
        );
    }

    @Test
    void hasHashEvenWithoutData() {
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
            new EOnumber().hashCode(),
            Matchers.greaterThan(0)
        );
    }

    @Test
    void hasDifferentHash() {
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
            new EOnumber().hashCode(),
            Matchers.not(new Data.ToPhi(0L).hashCode())
        );
    }

    @ParameterizedTest
    @ValueSource(classes = {
        EOnumber$EOdiv.class,
        EOnumber$EOgt.class,
        EOnumber$EOplus.class,
        EOnumber$EOtimes.class
    })
    void throwsCorrectErrorForXAttr(final Class<?> cls) {
        MatcherAssert.assertThat(
            "the message in the error is correct",
            Assertions.assertThrows(
                ExAbstract.class,
                () -> new Dataized(
                    new PhWith(
                        new PhWith(
                            (Phi) cls.getDeclaredConstructor().newInstance(),
                            Attr.RHO,
                            new Data.ToPhi(42)
                        ),
                        "x",
                        new Data.ToPhi(true)
                    )
                ).take(),
                "operation with TRUE fails with a proper message that explains what happened"
            ).getMessage(),
            Matchers.equalTo("the 'x' attribute must be a number")
        );
    }

    @ParameterizedTest
    @ValueSource(classes = {
        EOnumber$EOdiv.class,
        EOnumber$EOgt.class,
        EOnumber$EOplus.class,
        EOnumber$EOtimes.class,
        EOnumber$EOas_i64.class,
        EOnumber$EOfloor.class
    })
    void throwsCorrectErrorForRhoAttr(final Class<?> cls) {
        MatcherAssert.assertThat(
            "the message in the error is correct",
            Assertions.assertThrows(
                ExAbstract.class,
                () -> new Dataized(
                    new PhWith(
                        (Phi) cls.getDeclaredConstructor().newInstance(),
                        Attr.RHO,
                        new Data.ToPhi(true)
                    )
                ).take(),
                "EOnumber must be a number"
            ).getMessage(),
            Matchers.equalTo("the 'ρ' attribute must be a number")
        );
    }
}
