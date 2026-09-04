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
 * Test case for {@link Literal}.
 * @since 0.76.0
 */
final class LiteralTest {

    @Test
    void rendersNumberCarrier() {
        MatcherAssert.assertThat(
            "a number must render as the carrier application, but it didnt",
            new Literal("number", "40-45-00-00-00-00-00-00").phi(),
            Matchers.equalTo(
                "Φ.number(α0 ↦ Φ.bytes(α0 ↦ ⟦ Δ ⤍ 40-45-00-00-00-00-00-00 ⟧))"
            )
        );
    }

    @Test
    void rendersStringCarrier() {
        MatcherAssert.assertThat(
            "a string must render as the carrier application around its bytes, but it didnt",
            new Literal("string", "D0-B4-D1-80-D1-83-D0-B3").phi(),
            Matchers.equalTo(
                "Φ.string(α0 ↦ Φ.bytes(α0 ↦ ⟦ Δ ⤍ D0-B4-D1-80-D1-83-D0-B3 ⟧))"
            )
        );
    }

    @Test
    void rendersTruthAsDispatch() {
        MatcherAssert.assertThat(
            "the byte of truth must render as Φ.true, but it didnt",
            new Literal("bool", "01-").phi(),
            Matchers.equalTo("Φ.true")
        );
    }

    @Test
    void namesKeyWithForma() {
        MatcherAssert.assertThat(
            "the key must join the forma and the bytes, but it doesnt",
            new Literal("bytes", "0A-0B").key(),
            Matchers.equalTo("bytes:0A-0B")
        );
    }

    @Test
    void refusesStrangeForma() {
        Assertions.assertThrows(
            IllegalStateException.class,
            new Literal("tuple", "41-")::phi,
            "a forma with no carrier application cannot render, but it did"
        );
    }
}
