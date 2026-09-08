/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Anchored}.
 * @since 0.76.0
 */
final class AnchoredTest {

    @Test
    void prefixesDatumOfReceiverWithCarrier() {
        MatcherAssert.assertThat(
            "a receiver holding a datum must be keyed by the carrier of the operation, but it isnt",
            new Anchored(
                new Op("L_number_plus"),
                new Evaluation(
                    String.join(
                        "",
                        "L_number_plus\t⟦ x ↦ ∅, ρ ↦ ⟦ as-bytes ↦ Φ.bytes( data ↦ ",
                        "⟦ Δ ⤍ 40-00-00-00-00-00-00-00, ρ ↦ ∅ ⟧ ), φ ↦ ξ.as-bytes ⟧ ⟧"
                    )
                )
            ).receiver(),
            Matchers.equalTo("number:40-00-00-00-00-00-00-00")
        );
    }

    @Test
    void anchorsArgumentUnderPositionalName() {
        MatcherAssert.assertThat(
            "an argument shown under its α name must anchor all the same, but it didnt",
            new Anchored(
                new Op("L_number_plus"),
                new Evaluation(
                    String.join(
                        "",
                        "L_number_plus\t⟦ α0 ↦ Φ.number( α0 ↦ Φ.bytes( α0 ↦ ",
                        "⟦ Δ ⤍ 3F-F0-00-00-00-00-00-00, ρ ↦ ∅ ⟧ ) ), ρ ↦ ⟦ as-bytes ↦ Φ.bytes( ",
                        "data ↦ ⟦ λ ⤍ Sym_v0, ρ ↦ ∅ ⟧ ), φ ↦ ξ.as-bytes ⟧ ⟧"
                    )
                )
            ).arguments().get(),
            Matchers.contains("number:3F-F0-00-00-00-00-00-00")
        );
    }

    @Test
    void hasNoShapeWhileArgumentIsUnreduced() {
        MatcherAssert.assertThat(
            "a record whose argument is still an application has no shape yet, but it does",
            new Anchored(
                new Op("L_number_plus"),
                new Evaluation(
                    String.join(
                        "",
                        "L_number_plus\t⟦ x ↦ Φ.number( α0 ↦ Φ.bytes( α0 ↦ ⟦ λ ⤍ Sym_v0, ρ ↦ ∅ ⟧ ) )",
                        ".times( α0 ↦ Φ.number( α0 ↦ Φ.bytes( α0 ↦ ⟦ λ ⤍ Sym_v0, ρ ↦ ∅ ⟧ ) ) ), ",
                        "ρ ↦ ⟦ as-bytes ↦ Φ.bytes( data ↦ ⟦ λ ⤍ Sym_v1, ρ ↦ ∅ ⟧ ), φ ↦ ξ.as-bytes ⟧ ⟧"
                    )
                )
            ).shape().isPresent(),
            Matchers.is(false)
        );
    }
}
