/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.github.lombrozo.xnav.Xnav;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.w3c.dom.Element;

/**
 * Test case for {@link Stamp}.
 *
 * @since 0.77.0
 */
final class StampTest {

    @Test
    void marksPureFormationWithAtomOfItsCarrier() {
        final Element formation = (Element) new Xnav("<o name='f'><o base='∅' name='x'/></o>")
            .element("o").node();
        new Stamp("0a1b2c3d4e5f", "number", true).on(formation);
        MatcherAssert.assertThat(
            "the formation must carry the digest, the purity and the λ of a number, but it doesnt",
            new Xml(formation).text(),
            Matchers.equalTo(
                String.join(
                    "",
                    "<o lowered=\"0a1b2c3d4e5f\" name=\"f\" pure=\"true\">",
                    "<o base=\"∅\" name=\"x\"/><o atom=\"Φ.number\" name=\"λ\"/></o>"
                )
            )
        );
    }

    @Test
    void leavesImpureFormationWithoutPurity() {
        final Element formation = (Element) new Xnav("<o name='g'/>").element("o").node();
        new Stamp("f0e1d2c3b4a5", "bool", false).on(formation);
        MatcherAssert.assertThat(
            "an impure formation cannot be marked pure, but it is",
            formation.hasAttribute("pure"),
            Matchers.is(false)
        );
    }

    @Test
    void namesSiblingAfterDigest() {
        MatcherAssert.assertThat(
            "the sibling atom must be named after the digest, but it isnt",
            new Stamp("123456abcdef", "bytes", true).name(),
            Matchers.equalTo("l🌵123456abcdef")
        );
    }
}
