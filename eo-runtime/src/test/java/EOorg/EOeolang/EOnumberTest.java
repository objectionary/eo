/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

/*
 * @checkstyle PackageNameCheck (10 lines)
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package EOorg.EOeolang; // NOPMD

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
            "Hashes of the two instances should differ, but they didn't",
            new Data.ToPhi(42L).hashCode(),
            Matchers.not(Matchers.equalTo(new Data.ToPhi(42L).hashCode()))
        );
    }

    @Test
    void hasHashEvenWithoutData() {
        MatcherAssert.assertThat(
            "Object without data should have positive hash, but it didn't",
            new EOnumber().hashCode(),
            Matchers.greaterThan(0)
        );
    }

    @Test
    void hasDifferentHash() {
        MatcherAssert.assertThat(
            "Hashes of the two instances should differ, but they didn't",
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
    void throwsErrorForNonNumberXAttr(final Class<?> cls) throws Exception {
        Assertions.assertThrows(
            ExAbstract.class,
            () -> new Dataized(
                new PhWith(
                    new PhWith(
                        (Phi) cls.getDeclaredConstructor().newInstance(),
                        Phi.RHO,
                        new Data.ToPhi(42)
                    ),
                    "x",
                    new Data.ToPhi(true)
                )
            ).take(),
            "Operation with non-number x attribute should throw exception, but it didn't"
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
    void throwsErrorForNonNumberRhoAttr(final Class<?> cls) throws Exception {
        Assertions.assertThrows(
            ExAbstract.class,
            () -> new Dataized(
                new PhWith(
                    (Phi) cls.getDeclaredConstructor().newInstance(),
                    Phi.RHO,
                    new Data.ToPhi(true)
                )
            ).take(),
            "Operation with non-number rho attribute should throw exception, but it didn't"
        );
    }
}
