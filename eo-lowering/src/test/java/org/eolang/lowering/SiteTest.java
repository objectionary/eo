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
 * Test case for {@link Site}.
 *
 * @since 0.76.0
 */
final class SiteTest {

    @Test
    void rendersDispatchWithArguments() {
        MatcherAssert.assertThat(
            "the application must render receiver, method and arguments, but it didnt",
            new Site(
                "plus",
                new Symbol("v0", "number"),
                Collections.singletonList(
                    new Binding(
                        "α0", new Literal("number", "3F-F0-00-00-00-00-00-00")
                    )
                )
            ).phi(),
            Matchers.equalTo(
                String.join(
                    "",
                    "Φ.number(α0 ↦ Φ.bytes(α0 ↦ ⟦ λ ⤍ Sym_v0 ⟧))",
                    ".plus(α0 ↦ Φ.number(α0 ↦ Φ.bytes(α0 ↦ ⟦ Δ ⤍ 3F-F0-00-00-00-00-00-00 ⟧)))"
                )
            )
        );
    }

    @Test
    void rendersMarkerForMethodTheUniverseLacks() {
        MatcherAssert.assertThat(
            "a method the universe lacks must render as a marker phino parks on, but it doesnt",
            new Site(
                "minus",
                new Symbol("v0", "number"),
                Collections.singletonList(
                    new Binding("α0", new Literal("number", "3F-F0-00-00-00-00-00-00"))
                ),
                "number"
            ).phi(),
            Matchers.equalTo(
                String.join(
                    "",
                    "⟦ self ↦ Φ.number(α0 ↦ Φ.bytes(α0 ↦ ⟦ λ ⤍ Sym_v0 ⟧)), ",
                    "m🌵 ↦ ⟦ Δ ⤍ 6D-69-6E-75-73 ⟧, f🌵 ↦ ⟦ Δ ⤍ 6E-75-6D-62-65-72 ⟧, ",
                    "a0 ↦ Φ.number(α0 ↦ Φ.bytes(α0 ↦ ⟦ Δ ⤍ 3F-F0-00-00-00-00-00-00 ⟧)), ",
                    "λ ⤍ L_dispatch ⟧"
                )
            )
        );
    }

    @Test
    void leavesFormaSlotOutWhenUnwitnessed() {
        MatcherAssert.assertThat(
            "a marker of an unwitnessed value must carry no forma slot, but it does",
            new Site("neg", new Symbol("v0", "number"), Collections.emptyList()).phi(),
            Matchers.equalTo(
                String.join(
                    "",
                    "⟦ self ↦ Φ.number(α0 ↦ Φ.bytes(α0 ↦ ⟦ λ ⤍ Sym_v0 ⟧)), ",
                    "m🌵 ↦ ⟦ Δ ⤍ 6E-65-67 ⟧, λ ⤍ L_dispatch ⟧"
                )
            )
        );
    }

    @Test
    void rendersDispatchWhileReceiverIsUnsettled() {
        MatcherAssert.assertThat(
            "a method on a site still unsettled must render as the dispatch, but it doesnt",
            new Site(
                "minus",
                new Site("plus", new Symbol("v0", "number"), Collections.emptyList()),
                Collections.emptyList()
            ).phi(),
            Matchers.endsWith(".plus.minus")
        );
    }

    @Test
    void carriesNoKey() {
        MatcherAssert.assertThat(
            "an application has no value yet, but it names one",
            new Site(
                "size", new Symbol("v0", "bytes"), Collections.emptyList()
            ).key(),
            Matchers.emptyString()
        );
    }

    @Test
    void findsMatchingSiteDeep() {
        MatcherAssert.assertThat(
            "a matching site below the root must be found, but it wasnt",
            new Site(
                "plus",
                new Site(
                    "size", new Symbol("v0", "bytes"), Collections.emptyList()
                ),
                Collections.singletonList(
                    new Binding("α0", new Literal("number", "40-00-00-00-00-00-00-00"))
                )
            ).matches(
                new Shape(
                    "size", "sym:v0",
                    Collections.emptyList(), Collections.emptyList()
                )
            ),
            Matchers.is(true)
        );
    }

    @Test
    void swapsEveryMatchingSite() {
        MatcherAssert.assertThat(
            "both identical sites must give way to the one symbol, but they didnt",
            new Site(
                "plus",
                new Site("size", new Symbol("v0", "bytes"), Collections.emptyList()),
                Collections.singletonList(
                    new Binding(
                        "α0",
                        new Site("size", new Symbol("v0", "bytes"), Collections.emptyList())
                    )
                )
            ).swapped(
                new Shape(
                    "size", "sym:v0",
                    Collections.emptyList(), Collections.emptyList()
                ),
                new Symbol("s1", "number")
            ).phi(),
            Matchers.equalTo(
                String.join(
                    "",
                    "Φ.number(α0 ↦ Φ.bytes(α0 ↦ ⟦ λ ⤍ Sym_s1 ⟧))",
                    ".plus(α0 ↦ Φ.number(α0 ↦ Φ.bytes(α0 ↦ ⟦ λ ⤍ Sym_s1 ⟧)))"
                )
            )
        );
    }

    @Test
    void handsOutArgumentsOfMatchingSite() {
        MatcherAssert.assertThat(
            "the arguments of the site below the root must come back, but they didnt",
            new Site(
                "plus",
                new Site(
                    "if",
                    new Symbol("s1", "bool"),
                    Arrays.asList(
                        new Binding("α0", new Literal("number", "11-")),
                        new Binding("α1", new Symbol("v0", "number"))
                    )
                ),
                Collections.singletonList(
                    new Binding("α0", new Literal("number", "22-"))
                )
            ).arguments(
                new Shape("if", "sym:s1", Arrays.asList("t", "f"), Arrays.asList("", ""))
            ).get().get(1).value().key(),
            Matchers.equalTo("sym:v0")
        );
    }

    @Test
    void handsOutNothingForForeignShape() {
        MatcherAssert.assertThat(
            "a shape no site matches must find no arguments, but it did",
            new Site(
                "size", new Symbol("v0", "bytes"), Collections.emptyList()
            ).arguments(
                new Shape("if", "sym:v0", Arrays.asList("t", "f"), Arrays.asList("", ""))
            ).isPresent(),
            Matchers.is(false)
        );
    }

    @Test
    void keepsForeignSite() {
        MatcherAssert.assertThat(
            "a site of another shape must stay in place, but it didnt",
            new Site(
                "size", new Symbol("v1", "bytes"), Collections.emptyList()
            ).swapped(
                new Shape(
                    "size", "sym:v0",
                    Collections.emptyList(), Collections.emptyList()
                ),
                new Symbol("s1", "number")
            ).key(),
            Matchers.emptyString()
        );
    }
}
