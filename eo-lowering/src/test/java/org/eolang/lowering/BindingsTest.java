/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Bindings}.
 *
 * @since 0.77.0
 */
final class BindingsTest {

    @Test
    void readsBindingOfName() {
        MatcherAssert.assertThat(
            "the binding of a name must come back whole, but it didnt",
            new Bindings("⟦ x ↦ Φ.number( φ ↦ Φ.bytes( φ ↦ ⟦ λ ⤍ S2 ⟧ ) ), y ↦ ∅ ⟧").of("x"),
            Matchers.equalTo("Φ.number( φ ↦ Φ.bytes( φ ↦ ⟦ λ ⤍ S2 ⟧ ) )")
        );
    }

    @Test
    void splitsOnlyAtTopLevelCommas() {
        MatcherAssert.assertThat(
            "a comma inside a nested formation cannot split the binding, but it did",
            new Bindings("⟦ a ↦ ⟦ p ↦ ∅, q ↦ ∅ ⟧, b ↦ ⟦ Δ ⤍ 01- ⟧ ⟧").of("b"),
            Matchers.equalTo("⟦ Δ ⤍ 01- ⟧")
        );
    }

    @Test
    void answersBlankForUnboundName() {
        MatcherAssert.assertThat(
            "a name the body never binds must answer blank, but it didnt",
            new Bindings("⟦ x ↦ ∅ ⟧").of("z"),
            Matchers.is(Matchers.emptyString())
        );
    }

    @Test
    void collapsesWhitespaceAndNewlines() {
        MatcherAssert.assertThat(
            "a body spread over lines must still bind, but it didnt",
            new Bindings("⟦\n  x ↦\n    ⟦ Δ ⤍ FF- ⟧\n⟧").of("x"),
            Matchers.equalTo("⟦ Δ ⤍ FF- ⟧")
        );
    }
}
