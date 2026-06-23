/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link PhSticky}.
 * @since 0.60
 */
@SuppressWarnings("PMD.AvoidDuplicateLiterals")
final class PhStickyTest {

    @Test
    void memoizesThePredefinedExpression() {
        MatcherAssert.assertThat(
            "PhSticky must serve the very same instance for its one predefined expression, but it didnt",
            new PhSticky(new PhStickyTest.Formed("Φ.memoized")).take("alpha"),
            Matchers.sameInstance(
                new PhSticky(new PhStickyTest.Formed("Φ.memoized")).take("alpha")
            )
        );
    }

    @Test
    void rebuildsEveryOtherExpression() {
        MatcherAssert.assertThat(
            "PhSticky must rebuild every expression other than its one predefined key, but it didnt",
            new PhSticky(new PhStickyTest.Formed("Φ.elsewhere")).take("alpha"),
            Matchers.not(
                Matchers.sameInstance(
                    new PhSticky(new PhStickyTest.Formed("Φ.elsewhere")).take("alpha")
                )
            )
        );
    }

    /**
     * An object reporting a chosen forma, with one freshly-built attribute.
     * @since 0.60
     */
    static final class Formed extends PhDefault {

        /**
         * The forma this object reports.
         */
        private final String form;

        /**
         * Ctor.
         * @param forma The forma to report
         */
        Formed(final String forma) {
            super(new Attrs(new Attr("alpha", new AtComposite(Phi.Φ, rho -> new PhDefault()))));
            this.form = forma;
        }

        @Override
        public String forma() {
            return this.form;
        }
    }
}
