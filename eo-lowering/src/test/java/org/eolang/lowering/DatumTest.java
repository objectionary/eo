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
 * Test case for {@link Datum}.
 * @since 0.76.0
 */
final class DatumTest {

    @Test
    void namesNumberCarrier() {
        MatcherAssert.assertThat(
            "an arithmetic atom answers with a number term, but the forma says otherwise",
            new Datum(
                "40-22-00-00-00-00-00-00",
                "Φ.number( as-bytes ↦ Φ.bytes( data ↦ ⟦ Δ ⤍ 40-22-00-00-00-00-00-00, ρ ↦ ∅ ⟧ ) )"
            ).forma(),
            Matchers.equalTo("number")
        );
    }

    @Test
    void namesBoolCarrier() {
        MatcherAssert.assertThat(
            "a comparison atom answers with a bare bool reference, but the forma says otherwise",
            new Datum("00-", "Φ.false").forma(),
            Matchers.equalTo("bool")
        );
    }

    @Test
    void namesBytesCarrier() {
        MatcherAssert.assertThat(
            "a bytes atom answers with a bytes term, but the forma says otherwise",
            new Datum("F0-0F", "Φ.bytes( data ↦ ⟦ Δ ⤍ F0-0F, ρ ↦ ∅ ⟧ )").forma(),
            Matchers.equalTo("bytes")
        );
    }

    @Test
    void refusesFormaWhenNoAtomFired() {
        Assertions.assertThrows(
            IllegalStateException.class,
            new Datum("2A-", "")::forma,
            "a dataization without atom firings carries no forma, so guessing one is forbidden, but it wasnt"
        );
    }

    @Test
    void carriesBytesVerbatim() {
        MatcherAssert.assertThat(
            "the bytes must come back exactly as given, but they didnt",
            new Datum("DE-AD-BE-EF", "Φ.true").bytes(),
            Matchers.equalTo("DE-AD-BE-EF")
        );
    }
}
