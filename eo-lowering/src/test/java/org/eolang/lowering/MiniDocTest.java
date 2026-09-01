/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.jcabi.xml.XMLDocument;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link MiniDoc}.
 * @since 0.76.0
 */
final class MiniDocTest {

    @Test
    void rendersDispatchOnLiterals() {
        MatcherAssert.assertThat(
            "the fragment must become the φ of the root formation, but it didnt",
            new MiniDoc(
                new XMLDocument(
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
                )
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
    void carriesMethodTables() {
        MatcherAssert.assertThat(
            "the tables of the primitives must surround the fragment, but they dont",
            new MiniDoc(new XMLDocument("<o base='Φ.true'/>")).text(),
            Matchers.containsString("λ ⤍ L_number_plus")
        );
    }

    @Test
    void refusesContextDependentReference() {
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> new MiniDoc(new XMLDocument("<o base='ξ.x'/>")).text(),
            "a ξ reference means nothing outside its formation, so it cannot render, but it did"
        );
    }
}
