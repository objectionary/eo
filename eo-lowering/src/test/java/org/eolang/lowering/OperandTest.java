/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

/**
 * Test case for {@link Operand}.
 *
 * @since 0.77.0
 */
final class OperandTest {

    @ParameterizedTest
    @CsvSource(
        delimiter = '|',
        value = {
            "Φ.number( φ ↦ Φ.bytes( φ ↦ ⟦ λ ⤍ S2 ⟧ ) )|sym:S2",
            "Φ.number( φ ↦ Φ.bytes( φ ↦ ⟦ Δ ⤍ 40-08-00-00-00-00-00-00 ⟧ ) )|number:40-08-00-00-00-00-00-00",
            "Φ.string( as-bytes ↦ Φ.bytes( φ ↦ ⟦ Δ ⤍ 68-69 ⟧ ) )|string:68-69",
            "Φ.bytes( φ ↦ ⟦ Δ ⤍ -- ⟧ )|bytes:--",
            "Φ.bytes( φ ↦ ⟦ λ ⤍ S7, ρ ↦ ∅ ⟧ )|sym:S7",
            "Φ.bool( if ↦ ⟦ left ↦ ∅, right ↦ ∅, guard ↦ ⟦ λ ⤍ S4 ⟧, λ ⤍ L_fork ⟧ )|sym:S4",
            "⟦ left ↦ ∅, right ↦ ∅, guard ↦ ⟦ λ ⤍ S4 ⟧, λ ⤍ L_fork ⟧|sym:S4",
            "⟦ Δ ⤍ 01- ⟧|bytes:01-",
            "⟦ λ ⤍ S9 ⟧|sym:S9",
            "⟦ φ ↦ Φ.bytes( φ ↦ ⟦ Δ ⤍ 00- ⟧ ), ρ ↦ ∅ ⟧|bytes:00-",
            "⟦ if ↦ ⟦ left ↦ ∅, right ↦ ∅, guard ↦ ⟦ λ ⤍ S3 ⟧, λ ⤍ L_fork ⟧, ρ ↦ ∅ ⟧|sym:S3",
            "FF-|bytes:FF-"
        }
    )
    void readsKeyOfNormalizedOperand(final String phi, final String key) {
        MatcherAssert.assertThat(
            String.format("the operand '%s' must be read as '%s', but it wasnt", phi, key),
            new Operand(phi).key(),
            Matchers.equalTo(key)
        );
    }

    @Test
    void answersBlankForUnreadableOperand() {
        MatcherAssert.assertThat(
            "an operand that is neither a marker nor data cannot have a key, but it has",
            new Operand("Φ.number( φ ↦ ξ.ρ.x )").key(),
            Matchers.is(Matchers.emptyString())
        );
    }

    @Test
    void ignoresSurroundingWhitespace() {
        MatcherAssert.assertThat(
            "whitespace around and inside the operand cannot change its key, but it did",
            new Operand("  Φ.bytes(  φ ↦ ⟦ Δ ⤍ 2A- ⟧ )\n").key(),
            Matchers.equalTo("bytes:2A-")
        );
    }
}
