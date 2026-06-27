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
 * @since 0.74
 */
@SuppressWarnings("PMD.AvoidDuplicateLiterals")
final class PhStickyTest {

    @Test
    void memoizesTheTargetedExpressionOfNan() {
        MatcherAssert.assertThat(
            "PhSticky must serve the very same is-finite of nan across the program, but it didnt",
            Phi.Φ.take("nan").take("is-finite"),
            Matchers.sameInstance(
                Phi.Φ.take("nan").take("is-finite")
            )
        );
    }

    @Test
    void rebuildsExpressionWithAnotherKey() {
        MatcherAssert.assertThat(
            "PhSticky must rebuild an expression whose baked key is not the targeted one, but it didnt",
            new PhSticky(new PhStickyTest.Formed(), "Φ.elsewhere").take("is-finite"),
            Matchers.not(
                Matchers.sameInstance(
                    new PhSticky(new PhStickyTest.Formed(), "Φ.elsewhere").take("is-finite")
                )
            )
        );
    }

    @Test
    void rebuildsAnotherAttributeOfTheTargetedObject() {
        MatcherAssert.assertThat(
            "PhSticky must rebuild an attribute other than the targeted one, but it didnt",
            new PhSticky(new PhStickyTest.Formed(), "Φ.nan").take("as-bytes"),
            Matchers.not(
                Matchers.sameInstance(
                    new PhSticky(new PhStickyTest.Formed(), "Φ.nan").take("as-bytes")
                )
            )
        );
    }

    /**
     * An object with a couple of freshly-built attributes.
     * @since 0.74
     */
    static final class Formed extends PhDefault {

        /**
         * Ctor.
         */
        Formed() {
            super(
                new Attrs(
                    new Attr("is-finite", new AtComposite(Phi.Φ, rho -> new PhDefault())),
                    new Attr("as-bytes", new AtComposite(Phi.Φ, rho -> new PhDefault()))
                )
            );
        }
    }
}
