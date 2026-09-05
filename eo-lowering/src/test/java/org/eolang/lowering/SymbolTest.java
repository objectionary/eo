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
 * Test case for {@link Symbol}.
 * @since 0.76.0
 */
final class SymbolTest {

    @Test
    void rendersMarkedCarrier() {
        MatcherAssert.assertThat(
            "a symbolic number must render as a carrier holding a marker, but it didnt",
            new Symbol("v0", "number").phi(),
            Matchers.equalTo("Φ.number(α0 ↦ Φ.bytes(α0 ↦ ⟦ λ ⤍ Sym_v0 ⟧))")
        );
    }

    @Test
    void rendersMarkedStringCarrier() {
        MatcherAssert.assertThat(
            "a symbolic string must render as a string around a marked bytes, but it didnt",
            new Symbol("v1", "string").phi(),
            Matchers.equalTo("Φ.string(α0 ↦ Φ.bytes(α0 ↦ ⟦ λ ⤍ Sym_v1 ⟧))")
        );
    }

    @Test
    void namesKey() {
        MatcherAssert.assertThat(
            "the key must carry the name of the symbol, but it doesnt",
            new Symbol("s3", "bytes").key(),
            Matchers.equalTo("sym:s3")
        );
    }

    @Test
    void rendersMarkedBoolCarrier() {
        MatcherAssert.assertThat(
            "a symbolic bool must render as a bool around a marked bytes, but it didnt",
            new Symbol("s1", "bool").phi(),
            Matchers.equalTo("Φ.bool(α0 ↦ Φ.bytes(α0 ↦ ⟦ λ ⤍ Sym_s1 ⟧))")
        );
    }

    @Test
    void refusesUnmodelledCarrier() {
        Assertions.assertThrows(
            IllegalStateException.class,
            new Symbol("s2", "tuple")::phi,
            "a tuple has no symbolic carrier to render, but one rendered"
        );
    }
}
