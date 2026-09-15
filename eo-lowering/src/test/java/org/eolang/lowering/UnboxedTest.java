/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.github.lombrozo.xnav.Xnav;
import java.io.IOException;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.w3c.dom.Node;

/**
 * Test case for {@link Unboxed}.
 *
 * @since 0.77.0
 */
final class UnboxedTest {

    @Test
    void takesTheBoxOutOfTheFormation() throws IOException {
        MatcherAssert.assertThat(
            "the box must be gone from the lowered document, but it stayed",
            new Xml(
                new Unboxed(
                    UnboxedTest.doc(
                        String.join(
                            "",
                            "<o loc='Φ.foo' name='foo'><o loc='Φ.foo.g' name='g'>",
                            "<o base='∅' name='y'/><o name='λ'>L_box_p__foo__g</o></o></o>"
                        )
                    )
                ).copy()
            ).text(),
            Matchers.not(Matchers.containsString("L_box"))
        );
    }

    @Test
    void keepsTheVoidsOfTheFormation() throws IOException {
        MatcherAssert.assertThat(
            "the voids of an unboxed formation must stay, but they went with the box",
            new Xml(
                new Unboxed(
                    UnboxedTest.doc(
                        String.join(
                            "",
                            "<o loc='Φ.foo' name='foo'><o loc='Φ.foo.g' name='g'>",
                            "<o base='∅' name='y'/><o name='λ'>L_box_p__foo__g</o></o></o>"
                        )
                    )
                ).copy()
            ).text(),
            Matchers.containsString("<o base=\"∅\" name=\"y\"/>")
        );
    }

    @Test
    void keepsTheLambdaOfAnAtom() throws IOException {
        MatcherAssert.assertThat(
            "the λ of an atom is not a box and must stay, but it was taken out",
            new Xml(
                new Unboxed(
                    UnboxedTest.doc(
                        "<o loc='Φ.foo' name='foo'><o atom='Φ.number' name='λ'/></o>"
                    )
                ).copy()
            ).text(),
            Matchers.containsString("atom=\"Φ.number\"")
        );
    }

    @Test
    void keepsTheMarkerOfASymbol() throws IOException {
        MatcherAssert.assertThat(
            "the marker of a symbol is not a box and must stay, but it was taken out",
            new Xml(
                new Unboxed(
                    UnboxedTest.doc("<o loc='Φ.foo' name='foo'><o name='λ'>S4</o></o>")
                ).copy()
            ).text(),
            Matchers.containsString("S4")
        );
    }

    private static Node doc(final String body) {
        return new Xnav(String.format("<object>%s</object>", body))
            .element("object").node().getOwnerDocument();
    }
}
