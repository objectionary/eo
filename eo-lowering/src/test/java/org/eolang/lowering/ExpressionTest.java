/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.github.lombrozo.xnav.Xnav;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Expression}.
 * @since 0.76.0
 */
final class ExpressionTest {

    @Test
    void rendersDispatchOnLiterals() {
        MatcherAssert.assertThat(
            "the fragment must become the φ of the root formation, but it didnt",
            new Expression(
                new Xnav(
                    String.join(
                        "",
                        "<o base='.plus'>",
                        "<o base='Φ.number'>",
                        "<o as='α0' base='Φ.bytes'><o as='α0'>3F-F0-00-00-00-00-00-00</o></o>",
                        "</o>",
                        "<o as='α0' base='Φ.number'>",
                        "<o as='α0' base='Φ.bytes'><o as='α0'>40-00-00-00-00-00-00-00</o></o>",
                        "</o>",
                        "</o>"
                    )
                ).element("o")
            ).text(),
            Matchers.containsString(
                String.join(
                    "",
                    "φ ↦ Φ.number(α0 ↦ Φ.bytes(α0 ↦ ⟦ Δ ⤍ 3F-F0-00-00-00-00-00-00 ⟧))",
                    ".plus(α0 ↦ Φ.number(α0 ↦ Φ.bytes(α0 ↦ ⟦ Δ ⤍ 40-00-00-00-00-00-00-00 ⟧)))"
                )
            )
        );
    }

    @Test
    void refusesContextDependentReference() {
        Assertions.assertThrows(
            IllegalStateException.class,
            new Expression(new Xnav("<o base='ξ.x'/>").element("o"))::text,
            "a ξ reference means nothing outside its formation, so it cannot render, but it did"
        );
    }
}
