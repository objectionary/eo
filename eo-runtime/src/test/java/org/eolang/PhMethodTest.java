/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
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
            "Calling the same method twice should produce different objects, but it didn't",
            num.take("plus"),
            Matchers.not(Matchers.equalTo(num.take("plus")))
        );
    }

    @Test
    void calculatesPhiJustOnce() {
        final Dummy dummy = Dummy.make();
        final Phi phi = new PhMethod(dummy, "φ");
        for (int idx = 0; idx < 10; ++idx) {
            new Dataized(phi).take();
        }
        new Dataized(phi).take();
        MatcherAssert.assertThat(
            "Phi should be calculated only once, but it didn't",
            dummy.count,
            Matchers.equalTo(1)
        );
    }

    @Test
    void calculatesLocalJustOnce() {
        final Dummy dummy = Dummy.make();
        final Phi phi = new PhMethod(dummy, "foo");
        for (int idx = 0; idx < 10; ++idx) {
            new Dataized(phi).take();
        }
        new Dataized(phi).take();
        MatcherAssert.assertThat(
            "Foo should be calculated only once, but it wasn't",
            dummy.count,
            Matchers.equalTo(1)
        );
    }

    @Test
    void calculatesPhiOnce() {
        final Dummy dummy = Dummy.make();
        final Phi phi = new PhMethod(dummy, "neg");
        new Dataized(phi).take();
        new Dataized(phi).take();
        MatcherAssert.assertThat(
            "Neg should be calculated only once, but it wasn't",
            dummy.count,
            Matchers.equalTo(1)
        );
    }

    @Test
    void hasDifferentFormasWithOwnMethod() {
        final Phi dummy = Dummy.make();
        MatcherAssert.assertThat(
            "Forma of PhMethod should be differ from original, but it wasn't",
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
    public static final class Dummy extends PhDefault {
        /**
         * Count.
         */
        private int count;

        /**
         * Factory method.
         * @return New Dummy instance
         */
        static Dummy make() {
            final Dummy dummy = new Dummy();
            dummy.add(
                "φ",
                new AtOnce(
                    new AtComposite(
                        dummy,
                        self -> {
                            dummy.count += 1;
                            return new Data.ToPhi(1L);
                        }
                    )
                )
            );
            dummy.add(
                "foo",
                new AtComposite(
                    dummy,
                    self -> {
                        dummy.count += 1;
                        return new Data.ToPhi(1L);
                    }
                )
            );
            return dummy;
        }
    }
}
