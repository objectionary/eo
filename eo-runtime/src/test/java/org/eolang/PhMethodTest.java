/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link PhMethod}.
 *
 * @since 0.16
 */
final class PhMethodTest {
    @Test
    void comparesTwoObjects() {
        final Phi num = new Data.ToPhi(1L);
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
            num.take("plus"),
            Matchers.not(Matchers.equalTo(num.take("plus")))
        );
    }

    @Test
    void calculatesPhiJustOnce() {
        final Dummy dummy = new Dummy();
        final Phi phi = new PhMethod(dummy, "φ");
        final int total = 10;
        for (int idx = 0; idx < total; ++idx) {
            new Dataized(phi).take();
        }
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
            dummy.count,
            Matchers.equalTo(1)
        );
    }

    @Test
    void calculatesLocalJustOnce() {
        final Dummy dummy = new Dummy();
        final Phi phi = new PhMethod(dummy, "foo");
        final int total = 10;
        for (int idx = 0; idx < total; ++idx) {
            new Dataized(phi).take();
        }
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
            dummy.count,
            Matchers.equalTo(1)
        );
    }

    @Test
    void calculatesPhiOnce() {
        final Dummy dummy = new Dummy();
        final Phi phi = new PhMethod(dummy, "neg");
        new Dataized(phi).take();
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
            dummy.count,
            Matchers.equalTo(1)
        );
    }

    @Test
    void hasDifferentFormasWithOwnMethod() {
        final Phi dummy = new Dummy();
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
            dummy.forma(),
            Matchers.not(
                Matchers.equalTo(
                    new PhMethod(dummy, "foo").forma()
                )
            )
        );
    }

    /**
     * Dummy default.
     * @since 0.1.0
     */
    public static class Dummy extends PhDefault {
        /**
         * Count.
         */
        private int count;

        /**
         * Ctor.
         */
        @SuppressWarnings("PMD.ConstructorOnlyInitializesOrCallOtherConstructors")
        Dummy() {
            this.add(
                "φ",
                new AtOnce(
                    new AtComposite(
                        this,
                        self -> {
                            this.count += 1;
                            return new Data.ToPhi(1L);
                        }
                    )
                )
            );
            this.add(
                "foo",
                new AtComposite(
                    this,
                    self -> {
                        this.count += 1;
                        return new Data.ToPhi(1L);
                    }
                )
            );
        }
    }
}
