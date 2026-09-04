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

    @Test
    void canonicalizesNanNumber() {
        MatcherAssert.assertThat(
            "a NaN with the sign bit set must fold to the bytes of nan, but it didnt",
            new Datum(
                "FF-F8-00-00-00-00-00-00",
                "Φ.number( as-bytes ↦ Φ.bytes( data ↦ ⟦ Δ ⤍ FF-F8-00-00-00-00-00-00, ρ ↦ ∅ ⟧ ) )"
            ).bytes(),
            Matchers.equalTo("7F-F8-00-00-00-00-00-00")
        );
    }

    @Test
    void canonicalizesNanNumberWithPayload() {
        MatcherAssert.assertThat(
            "a NaN carrying a payload must fold to the bytes of nan, but it didnt",
            new Datum(
                "7F-F8-00-00-00-00-BE-EF",
                "Φ.number( as-bytes ↦ Φ.bytes( data ↦ ⟦ Δ ⤍ 7F-F8-00-00-00-00-BE-EF, ρ ↦ ∅ ⟧ ) )"
            ).bytes(),
            Matchers.equalTo("7F-F8-00-00-00-00-00-00")
        );
    }

    @Test
    void keepsInfinityVerbatim() {
        MatcherAssert.assertThat(
            "negative infinity is not a NaN, so its bytes must stay, but they didnt",
            new Datum(
                "FF-F0-00-00-00-00-00-00",
                "Φ.number( as-bytes ↦ Φ.bytes( data ↦ ⟦ Δ ⤍ FF-F0-00-00-00-00-00-00, ρ ↦ ∅ ⟧ ) )"
            ).bytes(),
            Matchers.equalTo("FF-F0-00-00-00-00-00-00")
        );
    }

    @Test
    void keepsNanBytesOutsideNumber() {
        MatcherAssert.assertThat(
            "bytes that merely look like a NaN are not a number, so they must stay, but they didnt",
            new Datum(
                "FF-F8-00-00-00-00-00-00",
                "Φ.bytes( data ↦ ⟦ Δ ⤍ FF-F8-00-00-00-00-00-00, ρ ↦ ∅ ⟧ )"
            ).bytes(),
            Matchers.equalTo("FF-F8-00-00-00-00-00-00")
        );
    }
}
