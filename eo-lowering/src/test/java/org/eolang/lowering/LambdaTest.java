/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Lambda}.
 *
 * @since 0.77.0
 */
final class LambdaTest {

    @Test
    void spellsLocatorOut() {
        MatcherAssert.assertThat(
            "the λ must spell the locator out, but it didnt",
            new Lambda("Φ.number.twice").name(),
            Matchers.equalTo("L_box_p__number__twice")
        );
    }

    @Test
    void spellsDashesOut() {
        MatcherAssert.assertThat(
            "a dash in a name must become a letter of its own, but it didnt",
            new Lambda("Φ.stdin.all-lines").name(),
            Matchers.equalTo("L_box_p__stdin__all_dlines")
        );
    }

    @Test
    void spellsTestMarkOut() {
        MatcherAssert.assertThat(
            "the mark of a test must become a letter of its own, but it didnt",
            new Lambda("Φ.bool.p🌵works").name(),
            Matchers.equalTo("L_box_p__bool__p_cworks")
        );
    }

    @Test
    void escapesWhateverItHasNoLetterFor() {
        MatcherAssert.assertThat(
            "a symbol outside the alphabet must be escaped, but it wasnt",
            new Lambda("Φ.foo.Ω").name(),
            Matchers.equalTo("L_box_p__foo___u0003a9")
        );
    }

    @Test
    void namesTwoLocatorsApart() {
        MatcherAssert.assertThat(
            "two locators that differ only in a dash must get two names, but they got one",
            new Lambda("Φ.foo.a-b").name(),
            Matchers.not(Matchers.equalTo(new Lambda("Φ.foo.a.b").name()))
        );
    }
}
