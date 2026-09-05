/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.util.Arrays;
import java.util.Collections;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Again}.
 * @since 0.76.0
 */
final class AgainTest {

    @Test
    void rendersMarkerAroundArguments() {
        MatcherAssert.assertThat(
            "the call must render as a formation of its arguments and the marker, but it doesnt",
            new Again(
                Arrays.asList(new Symbol("v0", "number"), new Literal("number", "11-"))
            ).phi(),
            Matchers.equalTo(
                String.join(
                    "",
                    "⟦ a0 ↦ Φ.number(α0 ↦ Φ.bytes(α0 ↦ ⟦ λ ⤍ Sym_v0 ⟧)), ",
                    "a1 ↦ Φ.number(α0 ↦ Φ.bytes(α0 ↦ ⟦ Δ ⤍ 11- ⟧)), λ ⤍ L_self ⟧"
                )
            )
        );
    }

    @Test
    void carriesNoKey() {
        MatcherAssert.assertThat(
            "a call to itself is no value, but it names one",
            new Again(Collections.singletonList(new Symbol("v0", "number"))).key(),
            Matchers.emptyString()
        );
    }

    @Test
    void handsOutArguments() {
        MatcherAssert.assertThat(
            "the arguments must come back in their order, but they dont",
            new Again(
                Arrays.asList(new Symbol("v1", "number"), new Symbol("v0", "number"))
            ).again().get().get(0).key(),
            Matchers.equalTo("sym:v1")
        );
    }

    @Test
    void swapsSiteInsideArgument() {
        MatcherAssert.assertThat(
            "a site inside an argument must give way to the symbol, but it didnt",
            new Again(
                Collections.singletonList(
                    new Site("size", new Symbol("v0", "bytes"), Collections.emptyList())
                )
            ).swapped(
                new Shape("size", "sym:v0", Collections.emptyList(), Collections.emptyList()),
                new Symbol("s1", "number")
            ).phi(),
            Matchers.containsString("a0 ↦ Φ.number(α0 ↦ Φ.bytes(α0 ↦ ⟦ λ ⤍ Sym_s1 ⟧))")
        );
    }
}
