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
 * Test case for {@link Operand}.
 *
 * <p>The fixtures are verbatim record fields of phino 0.0.114, so these
 * tests also pin the rendering this module relies on: if an upgrade of
 * phino reshapes its records, the anchoring breaks here first.</p>
 *
 * @since 0.76.0
 */
final class OperandTest {

    @Test
    void anchorsNumberLiteral() {
        MatcherAssert.assertThat(
            "a number carrier wrapping a datum must anchor to its bytes, but it didnt",
            new Operand(
                "Φ.number( as-bytes ↦ Φ.bytes( data ↦ ⟦ Δ ⤍ 40-00-00-00-00-00-00-00, ρ ↦ ∅ ⟧ ) )"
            ).key(),
            Matchers.equalTo("number:40-00-00-00-00-00-00-00")
        );
    }

    @Test
    void anchorsPositionallyNamedLiteral() {
        MatcherAssert.assertThat(
            "a carrier with unresolved α names must anchor all the same, but it didnt",
            new Operand(
                "Φ.number( α0 ↦ Φ.bytes( α0 ↦ ⟦ Δ ⤍ 3F-F0-00-00-00-00-00-00, ρ ↦ ∅ ⟧ ) )"
            ).key(),
            Matchers.equalTo("number:3F-F0-00-00-00-00-00-00")
        );
    }

    @Test
    void anchorsMarkedNumber() {
        MatcherAssert.assertThat(
            "a number carrier wrapping a marker must anchor to its name, but it didnt",
            new Operand(
                "Φ.number( as-bytes ↦ Φ.bytes( data ↦ ⟦ λ ⤍ Sym_v0, ρ ↦ ∅ ⟧ ) )"
            ).key(),
            Matchers.equalTo("sym:v0")
        );
    }

    @Test
    void anchorsStringLiteral() {
        MatcherAssert.assertThat(
            "a string carrier wrapping a datum must anchor to its bytes, but it didnt",
            new Operand(
                "Φ.string( as-bytes ↦ Φ.bytes( data ↦ ⟦ Δ ⤍ 61-62-63, ρ ↦ ∅ ⟧ ) )"
            ).key(),
            Matchers.equalTo("string:61-62-63")
        );
    }

    @Test
    void anchorsMarkedString() {
        MatcherAssert.assertThat(
            "a string carrier wrapping a marker must anchor to its name, but it didnt",
            new Operand(
                "Φ.string( α0 ↦ Φ.bytes( α0 ↦ ⟦ λ ⤍ Sym_v2, ρ ↦ ∅ ⟧ ) )"
            ).key(),
            Matchers.equalTo("sym:v2")
        );
    }

    @Test
    void anchorsBytesLiteral() {
        MatcherAssert.assertThat(
            "a bytes carrier wrapping a datum must anchor to its bytes, but it didnt",
            new Operand("Φ.bytes( data ↦ ⟦ Δ ⤍ 01-02, ρ ↦ ∅ ⟧ )").key(),
            Matchers.equalTo("bytes:01-02")
        );
    }

    @Test
    void anchorsBareTruth() {
        MatcherAssert.assertThat(
            "a bare truth must anchor to its byte, but it didnt",
            new Operand("Φ.true").key(),
            Matchers.equalTo("bool:01-")
        );
    }

    @Test
    void rejectsUnreducedApplication() {
        MatcherAssert.assertThat(
            "an application still to reduce cannot anchor, but it did",
            new Operand(
                "Φ.number( α0 ↦ Φ.bytes( α0 ↦ ⟦ λ ⤍ Sym_v0, ρ ↦ ∅ ⟧ ) ).times( x ↦ Φ.number( α0 ↦ Φ.bytes( α0 ↦ ⟦ Δ ⤍ 40-08-00-00-00-00-00-00, ρ ↦ ∅ ⟧ ) ) )"
            ).anchored(),
            Matchers.is(false)
        );
    }

    @Test
    void refusesKeyOfUnanchoredTerm() {
        Assertions.assertThrows(
            IllegalStateException.class,
            new Operand("ξ.as-bytes")::key,
            "a term of no value shape cannot answer a key, but it did"
        );
    }
}
