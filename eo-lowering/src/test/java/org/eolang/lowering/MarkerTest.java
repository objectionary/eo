/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

/**
 * Test case for {@link Marker}.
 *
 * @since 0.77.0
 */
final class MarkerTest {

    @ParameterizedTest
    @CsvSource(
        delimiter = '|',
        value = {
            "sym:S1|number|Φ.number( φ ↦ Φ.bytes( φ ↦ ⟦ λ ⤍ S1 ⟧ ) )",
            "sym:S2|string|Φ.string( φ ↦ Φ.bytes( φ ↦ ⟦ λ ⤍ S2 ⟧ ) )",
            "sym:S3|bytes|Φ.bytes( φ ↦ ⟦ λ ⤍ S3 ⟧ )",
            "sym:S4|bool|Φ.bool( if ↦ ⟦ left ↦ ∅, right ↦ ∅, guard ↦ ⟦ λ ⤍ S4 ⟧, λ ⤍ L_fork ⟧ )",
            "sym:S5|object|⟦ λ ⤍ S5 ⟧",
            "number:40-08-00-00-00-00-00-00|number|Φ.number( φ ↦ Φ.bytes( φ ↦ ⟦ Δ ⤍ 40-08-00-00-00-00-00-00 ⟧ ) )",
            "bytes:--|bytes|Φ.bytes( φ ↦ ⟦ Δ ⤍ -- ⟧ )",
            "bool:00-|bool|Φ.false",
            "bool:FF-|bool|Φ.true"
        }
    )
    void spellsMarkerOfCarrier(final String key, final String carrier, final String phi) {
        MatcherAssert.assertThat(
            String.format("the %s marker of '%s' must be spelled right, but it wasnt", carrier, key),
            new Marker(key, carrier).phi(),
            Matchers.equalTo(phi)
        );
    }

    @Test
    void refusesTupleWithoutParts() {
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> new Marker("sym:S6", "tuple").phi(),
            "a tuple needs its parts spelled, which a bare marker cannot do, but it did"
        );
    }
}
