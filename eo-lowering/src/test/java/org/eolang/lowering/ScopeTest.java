/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.github.lombrozo.xnav.Xnav;
import java.util.Arrays;
import java.util.Collections;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Scope}.
 * @since 0.76.0
 */
final class ScopeTest {

    @Test
    void bindsRootVoidsPositionally() {
        MatcherAssert.assertThat(
            "a void of the formation must be its positional symbol, but it isnt",
            ScopeTest.root().term("x").get().key(),
            Matchers.equalTo("sym:v0")
        );
    }

    @Test
    void bindsArgumentByPosition() {
        MatcherAssert.assertThat(
            "the first argument must bind the first void after ρ, but it doesnt",
            ScopeTest.root().inside(
                ScopeTest.helper(),
                Collections.singletonList(
                    new Binding("α0", new Literal("number", "40-00-00-00-00-00-00-00"))
                )
            ).term("i").get().key(),
            Matchers.equalTo("number:40-00-00-00-00-00-00-00")
        );
    }

    @Test
    void bindsArgumentByName() {
        MatcherAssert.assertThat(
            "an argument named after a void must bind that void, but it doesnt",
            ScopeTest.root().inside(
                ScopeTest.helper(),
                Collections.singletonList(new Binding("i", new Symbol("v0", "number")))
            ).term("i").get().key(),
            Matchers.equalTo("sym:v0")
        );
    }

    @Test
    void leadsBackThroughRho() {
        MatcherAssert.assertThat(
            "ρ inside a helper must lead to the scope it was applied from, but it doesnt",
            ScopeTest.root().inside(
                ScopeTest.helper(),
                Collections.singletonList(new Binding("α0", new Symbol("v0", "number")))
            ).above().term("x").get().key(),
            Matchers.equalTo("sym:v0")
        );
    }

    @Test
    void refusesRhoAtTheRoot() {
        Assertions.assertThrows(
            IllegalStateException.class,
            ScopeTest.root()::above,
            "the root has nothing above it to read, but it answered"
        );
    }

    @Test
    void refusesPartialApplication() {
        MatcherAssert.assertThat(
            "a helper applied to fewer arguments than voids must refuse, but it didnt",
            Assertions.assertThrows(
                IllegalStateException.class,
                () -> ScopeTest.root().inside(ScopeTest.helper(), Collections.emptyList()),
                "a helper with an unbound void was applied, but it must not be"
            ).getMessage(),
            Matchers.containsString("binds 0 of them")
        );
    }

    @Test
    void refusesArgumentBeyondVoids() {
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> ScopeTest.root().inside(
                ScopeTest.helper(),
                Arrays.asList(
                    new Binding("α0", new Symbol("v0", "number")),
                    new Binding("α1", new Symbol("v0", "number"))
                )
            ),
            "a helper handed more arguments than it has voids was applied, but it must not be"
        );
    }

    @Test
    void refusesArgumentOfUnknownName() {
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> ScopeTest.root().inside(
                ScopeTest.helper(),
                Collections.singletonList(new Binding("k", new Symbol("v0", "number")))
            ),
            "an argument naming no void of the helper was bound, but it must not be"
        );
    }

    private static Scope root() {
        return new Scope(Collections.singletonMap("x", "number"), "f", Collections.emptyMap());
    }

    private static Xnav helper() {
        return new Xnav(
            String.join(
                "",
                "<o name='a🌵3-4'><o base='∅' name='ρ'/><o base='∅' name='i'/>",
                "<o base='ξ.ρ.x.times' name='φ'><o as='α0' base='ξ.i'/></o></o>"
            )
        ).element("o");
    }
}
