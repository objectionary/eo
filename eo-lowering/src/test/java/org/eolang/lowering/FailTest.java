/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.util.Collections;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Fail}.
 *
 * @since 0.76.0
 */
final class FailTest {

    @Test
    void rendersMarkerAroundReason() {
        MatcherAssert.assertThat(
            "the terminator must render as a formation of its reason and the marker, but it doesnt",
            new Fail(new Literal("string", "6F-70-73")).phi(),
            Matchers.equalTo(
                "⟦ r ↦ Φ.string(α0 ↦ Φ.bytes(α0 ↦ ⟦ Δ ⤍ 6F-70-73 ⟧)), λ ⤍ L_fail ⟧"
            )
        );
    }

    @Test
    void carriesNoKey() {
        MatcherAssert.assertThat(
            "a terminator is no value, but it names one",
            new Fail(new Literal("string", "6F-70-73")).key(),
            Matchers.emptyString()
        );
    }

    @Test
    void knowsItself() {
        MatcherAssert.assertThat(
            "the terminator must hand itself out as the failure it is, but it doesnt",
            new Fail(new Symbol("v0", "string")).terminator().get().reason().key(),
            Matchers.equalTo("sym:v0")
        );
    }

    @Test
    void isNoCall() {
        MatcherAssert.assertThat(
            "a terminator is no call to itself, but it claims to be one",
            new Fail(new Symbol("v0", "string")).again().isPresent(),
            Matchers.is(false)
        );
    }

    @Test
    void swapsSiteInsideReason() {
        MatcherAssert.assertThat(
            "a site inside the reason must give way to the symbol, but it didnt",
            new Fail(
                new Site("concat", new Symbol("v0", "string"), Collections.emptyList())
            ).swapped(
                new Shape("concat", "sym:v0", Collections.emptyList(), Collections.emptyList()),
                new Symbol("s1", "bytes")
            ).phi(),
            Matchers.containsString("r ↦ Φ.bytes(α0 ↦ ⟦ λ ⤍ Sym_s1 ⟧)")
        );
    }
}
