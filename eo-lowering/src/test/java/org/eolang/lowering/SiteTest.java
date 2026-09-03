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
 * Test case for {@link Site}.
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
