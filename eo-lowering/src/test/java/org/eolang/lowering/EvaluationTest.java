/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Evaluation}.
 * @since 0.76.0
 */
final class EvaluationTest {

    @Test
    void tellsParkedFromFired() {
        MatcherAssert.assertThat(
            "a record of two fields must read as parked, but it doesnt",
            new Evaluation("L_number_plus\t⟦ x ↦ ∅, ρ ↦ ∅ ⟧").parked(),
            Matchers.is(true)
        );
    }

    @Test
    void readsResultOfFiredAtom() {
        MatcherAssert.assertThat(
            "the third field must come back as the result, but it didnt",
            new Evaluation("L_number_gt\t⟦ x ↦ ∅, ρ ↦ ∅ ⟧\tΦ.true").result(),
            Matchers.equalTo("Φ.true")
        );
    }

    @Test
    void refusesResultOfParkedAtom() {
        Assertions.assertThrows(
            IllegalStateException.class,
            new Evaluation("L_number_plus\t⟦ x ↦ ∅, ρ ↦ ∅ ⟧")::result,
            "a parked atom has no result to read, but one came back"
        );
    }

    @Test
    void splitsBindingsAtOutermostCommas() {
        MatcherAssert.assertThat(
            "the commas inside a nested term must not split it, but they did",
            new Evaluation(
                "L_number_plus\t⟦ x ↦ Φ.number( as-bytes ↦ Φ.bytes( data ↦ ⟦ Δ ⤍ 3F-F0-00-00-00-00-00-00, ρ ↦ ∅ ⟧ ) ), ρ ↦ ⟦ as-bytes ↦ Φ.bytes( data ↦ ⟦ λ ⤍ Sym_v0, ρ ↦ ∅ ⟧ ), φ ↦ ξ.as-bytes ⟧ ⟧"
            ).bindings(),
            Matchers.hasEntry(
                Matchers.equalTo("x"),
                Matchers.startsWith("Φ.number( as-bytes")
            )
        );
    }

    @Test
    void skipsUnsetBinding() {
        MatcherAssert.assertThat(
            "a binding still holding ∅ must not count, but it does",
            new Evaluation(
                "L_bytes_slice\t⟦ start ↦ Φ.true, len ↦ Φ.true, cant-slice ↦ ∅, ρ ↦ ∅ ⟧"
            ).bindings().size(),
            Matchers.equalTo(2)
        );
    }

    @Test
    void findsMarkerOfReceiver() {
        MatcherAssert.assertThat(
            "the first marker inside ρ must name the receiver, but it didnt",
            new Evaluation(
                "L_number_plus\t⟦ x ↦ ∅, ρ ↦ ⟦ as-bytes ↦ Φ.bytes( data ↦ ⟦ λ ⤍ Sym_v0, ρ ↦ ∅ ⟧ ), φ ↦ ξ.as-bytes ⟧ ⟧"
            ).receiver(),
            Matchers.equalTo("sym:v0")
        );
    }

    @Test
    void findsDatumOfReceiver() {
        MatcherAssert.assertThat(
            "the first datum inside ρ must name the receiver, but it didnt",
            new Evaluation(
                "L_number_times\t⟦ x ↦ ∅, ρ ↦ ⟦ as-bytes ↦ Φ.bytes( data ↦ ⟦ Δ ⤍ 40-00-00-00-00-00-00-00, ρ ↦ ∅ ⟧ ), φ ↦ ξ.as-bytes ⟧ ⟧"
            ).receiver(),
            Matchers.equalTo("Δ:40-00-00-00-00-00-00-00")
        );
    }

    @Test
    void refusesReceiverOfNoIdentity() {
        Assertions.assertThrows(
            IllegalStateException.class,
            new Evaluation("L_number_plus\t⟦ x ↦ ∅, ρ ↦ ⟦ φ ↦ ξ.x ⟧ ⟧")::receiver,
            "a receiver holding no value cannot be identified, but it was"
        );
    }
}
