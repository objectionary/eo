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
 * Test case for {@link Splice}.
 *
 * @since 0.77.0
 */
final class SpliceTest {

    @Test
    void importsReducedApplicationKeepingItsPosition() {
        final Element written = SpliceTest.element(
            "<o loc='Φ.f' name='f'><o base='∅' name='x'/><o base='ξ.x.plus' line='3' name='φ'><o as='α0' base='Φ.number'/></o></o>"
        );
        new Splice(
            written,
            SpliceTest.element(
                "<o><o base='∅' name='x'/><o base='Φ.number' name='φ'><o as='φ'><o name='λ'>S2</o></o></o></o>"
            )
        ).apply();
        MatcherAssert.assertThat(
            "the reduced application must stand where the written one stood, with its line, but it doesnt",
            new Xml(written).text(),
            Matchers.containsString(
                "<o base=\"Φ.number\" line=\"3\" name=\"φ\"><o as=\"φ\"><o name=\"λ\">S2</o></o></o>"
            )
        );
    }

    @Test
    void keepsVoidsTheResidualLacks() {
        final Element written = SpliceTest.element(
            "<o name='f'><o base='∅' name='ρ'/><o base='∅' name='x'/><o base='ξ.x' name='φ'/></o>"
        );
        new Splice(
            written,
            SpliceTest.element("<o><o base='∅' name='x'/><o base='ξ.x' name='φ'/></o>")
        ).apply();
        MatcherAssert.assertThat(
            "the ρ void phino hides must stay as written, but it is gone",
            new Xml(written).text(),
            Matchers.containsString("<o base=\"∅\" name=\"ρ\"/>")
        );
    }

    @Test
    void recursesIntoNestedFormationWithoutItsBox() {
        final Element written = SpliceTest.element(
            "<o name='f'><o loc='Φ.f.g' name='g'><o base='∅' name='y'/><o base='ξ.y.plus' name='φ'><o as='α0' base='Φ.number'/></o></o></o>"
        );
        new Splice(
            written,
            SpliceTest.element(
                "<o><o name='g'><o base='∅' name='y'/><o base='Φ.number' name='φ'><o as='φ'><o name='λ'>S3</o></o></o><o name='λ'>L_box_2</o></o></o>"
            )
        ).apply();
        MatcherAssert.assertThat(
            "the body of the nested formation must be imported without the box, but it isnt",
            new Xml(written).text(),
            Matchers.allOf(
                Matchers.containsString("<o name=\"λ\">S3</o>"),
                Matchers.not(Matchers.containsString("L_box_2"))
            )
        );
    }

    @Test
    void stripsCommentsInsideImportedMarker() {
        final Element written = SpliceTest.element(
            "<o name='f'><o base='ξ.x.plus' name='φ'><o as='α0' base='Φ.number'/></o></o>"
        );
        new Splice(
            written,
            SpliceTest.element(
                "<o><o base='Φ.number' name='φ'><!-- 5 --><o as='φ' base='Φ.bytes'><o as='φ'><o name='λ'>S2</o></o></o></o></o>"
            )
        ).apply();
        MatcherAssert.assertThat(
            "the marker must come without the comment phino puts on a literal, but it didnt",
            new Xml(written).text(),
            Matchers.not(Matchers.containsString("<!--"))
        );
    }

    @Test
    void leavesBindingTheResidualLacks() {
        final Element written = SpliceTest.element(
            "<o name='f'><o base='Φ.q' name='z'/><o base='ξ.z' name='φ'/></o>"
        );
        new Splice(written, SpliceTest.element("<o><o base='ξ.z' name='φ'/></o>")).apply();
        MatcherAssert.assertThat(
            "a binding phino did not print must stay as written, but it is gone",
            new Xml(written).text(),
            Matchers.containsString("<o base=\"Φ.q\" name=\"z\"/>")
        );
    }

    private static Element element(final String xml) {
        return (Element) new Xnav(String.format("<object>%s</object>", xml))
            .element("object").element("o").node();
    }


    @Test
    void keepsBindingReducedOnlyInPart() {
        final Element written = SpliceTest.element(
            "<o name='f'><o base='∅' name='x'/><o base='ξ.x.gt.if' name='φ'><o as='α0' base='Φ.number'/></o></o>"
        );
        new Splice(
            written,
            SpliceTest.element(
                String.join(
                    "",
                    "<o><o base='∅' name='x'/><o base='ξ.a.if' name='φ'><o as='α0' base='Φ.number'>",
                    "<o as='φ' base='Φ.bytes'><o as='φ'><o name='λ'>S2</o></o></o></o></o></o>"
                )
            )
        ).apply();
        MatcherAssert.assertThat(
            "a binding phino reduced only in part must stay as written, but it was replaced",
            new Xml(written).text(),
            Matchers.containsString("<o base=\"ξ.x.gt.if\" name=\"φ\">")
        );
    }

    @Test
    void keepsLocalNameOfReplacedBinding() {
        final Element written = SpliceTest.element(
            "<o name='f'><o base='∅' name='x'/><o base='ξ.x.plus' local='sum' name='a🌵3-4'><o as='α0' base='Φ.number'/></o></o>"
        );
        new Splice(
            written,
            SpliceTest.element(
                "<o><o base='∅' name='x'/><o base='Φ.number' name='a🌵3-4'><o as='φ' base='Φ.bytes'><o as='φ'><o name='λ'>S2</o></o></o></o></o>"
            )
        ).apply();
        MatcherAssert.assertThat(
            "the local name of a replaced binding must survive, but it is gone",
            new Xml(written).text(),
            Matchers.containsString("local=\"sum\"")
        );
    }
}
