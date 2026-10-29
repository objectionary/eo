/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.github.lombrozo.xnav.Filter;
import com.github.lombrozo.xnav.Xnav;
import java.util.HashMap;
import java.util.Map;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Shapes}.
 *
 * @since 0.76.0
 */
final class ShapesTest {

    @Test
    void showsTheVoidsOfAFormationInBrackets() {
        MatcherAssert.assertThat(
            "the voids must be shown in brackets, a caret for ρ, but they werent",
            ShapesTest.shapes(
                String.join(
                    "",
                    "<object><o loc='Φ.mill' name='mill'>",
                    "<o loc='Φ.mill.a🌵2-4' name='a🌵2-4' line='2'>",
                    "<o base='∅' name='grain' loc='Φ.mill.a🌵2-4.grain'/>",
                    "<o base='∅' name='ρ' loc='Φ.mill.a🌵2-4.ρ'/>",
                    "</o></o></object>"
                ),
                "<answers/>"
            ).shown("Φ.mill.a🌵2-4", false),
            Matchers.equalTo("[∅ ^]")
        );
    }

    @Test
    void showsWhatItsDecorateeAnswers() {
        MatcherAssert.assertThat(
            "what the @ answers must follow the voids, but it didnt",
            ShapesTest.shapes(
                "<object><o loc='Φ.mill.α0' line='3'><o base='∅' name='x' loc='Φ.mill.α0.x'/></o></object>",
                "<answers><o loc='Φ.mill.α0.φ' copies='3'><is loc='Φ.string'/></o></answers>"
            ).shown("Φ.mill.α0", false),
            Matchers.equalTo("[∅] → Φ.string")
        );
    }

    @Test
    void callsItselfARoundArrow() {
        MatcherAssert.assertThat(
            "an attribute that gives the formation back must say so with ↺, but it didnt",
            ShapesTest.shapes(
                String.join(
                    "",
                    "<object><o loc='Φ.mill.α0' line='3'>",
                    "<o base='∅' name='x' loc='Φ.mill.α0.x'/>",
                    "<o base='.slice' name='read' loc='Φ.mill.α0.read'/>",
                    "</o></object>"
                ),
                "<answers><o loc='Φ.mill.α0.read' copies='2'><is loc='Φ.mill.α0'/></o></answers>"
            ).shown("Φ.mill.α0", false),
            Matchers.equalTo("[∅]\n  read ↺")
        );
    }

    @Test
    void stepsOnceIntoAnAnswerWithNoNameOfItsOwn() {
        MatcherAssert.assertThat(
            "an answer with no name must show its voids and what its @ answers, but it didnt",
            ShapesTest.shapes(
                String.join(
                    "",
                    "<object><o loc='Φ.mill.α0' line='3'><o base='∅' name='x' loc='Φ.mill.α0.x'/></o>",
                    "<o loc='Φ.mill.p🌵7-2' line='7'>",
                    "<o base='∅' name='ρ' loc='Φ.mill.p🌵7-2.ρ'/></o></object>"
                ),
                String.join(
                    "",
                    "<answers><o loc='Φ.mill.α0.φ' copies='1'><is loc='Φ.mill.p🌵7-2'/></o>",
                    "<o loc='Φ.mill.p🌵7-2.φ' copies='1'><is loc='Φ.number'/></o></answers>"
                )
            ).shown("Φ.mill.α0", false),
            Matchers.equalTo("[∅] → [^] → Φ.number")
        );
    }

    @Test
    void wrapsAShapeAmongOtherAnswersInParentheses() {
        MatcherAssert.assertThat(
            "a shape among several answers must be wrapped in parentheses, but it wasnt",
            ShapesTest.shapes(
                String.join(
                    "",
                    "<object><o loc='Φ.mill.α0' line='3'><o base='∅' name='x' loc='Φ.mill.α0.x'/></o>",
                    "<o loc='Φ.mill.p🌵7-2' line='7'/></object>"
                ),
                String.join(
                    "",
                    "<answers><o loc='Φ.mill.α0.φ' copies='5'>",
                    "<set copies='4'><is loc='Φ.mill.p🌵7-2'/></set>",
                    "<set copies='1'><is loc='Φ.bytes'/></set></o>",
                    "<o loc='Φ.mill.p🌵7-2.φ' copies='1'><data/></o></answers>"
                )
            ).shown("Φ.mill.α0", false),
            Matchers.equalTo("[∅] → ([] → a datum) or Φ.bytes")
        );
    }

    @Test
    void capsTheAnswersPastThree() {
        MatcherAssert.assertThat(
            "four answers under a mark must be counted, not listed, but they were listed",
            ShapesTest.shapes(
                "<object><o loc='Φ.mill.α1' line='4'/></object>",
                String.join(
                    "",
                    "<answers><o loc='Φ.mill.α1.φ' copies='4'>",
                    "<is loc='Φ.oak'/><is loc='Φ.elm'/><is loc='Φ.ash'/><is loc='Φ.fir'/>",
                    "</o></answers>"
                )
            ).shown("Φ.mill.α1", true),
            Matchers.equalTo("[] → 4 answers")
        );
    }

    @Test
    void capsTheAttributesPastFour() {
        MatcherAssert.assertThat(
            "a fifth attribute under a mark must be counted, not listed, but it was listed",
            ShapesTest.shapes(
                String.join(
                    "",
                    "<object><o loc='Φ.mill.α1' line='4'>",
                    "<o base='Φ.oak' name='oak' loc='Φ.mill.α1.oak'/>",
                    "<o base='Φ.elm' name='elm' loc='Φ.mill.α1.elm'/>",
                    "<o base='Φ.ash' name='ash' loc='Φ.mill.α1.ash'/>",
                    "<o base='Φ.fir' name='fir' loc='Φ.mill.α1.fir'/>",
                    "<o base='Φ.yew' name='yew' loc='Φ.mill.α1.yew'/>",
                    "</o></object>"
                ),
                "<answers/>"
            ).shown("Φ.mill.α1", true),
            Matchers.endsWith("\n  and 1 more")
        );
    }

    @Test
    void foldsFormationsOfOneShapeIntoOneLabel() {
        MatcherAssert.assertThat(
            "two formations of one shape must fold into one label, but they didnt",
            ShapesTest.shapes(
                String.join(
                    "",
                    "<object><o loc='Φ.mill.a🌵2-4' line='2'><o base='∅' name='x' loc='Φ.mill.a🌵2-4.x'/></o>",
                    "<o loc='Φ.mill.a🌵5-4' line='5'><o base='∅' name='y' loc='Φ.mill.a🌵5-4.y'/></o>",
                    "</object>"
                ),
                String.join(
                    "",
                    "<answers><o loc='Φ.mill.sack' copies='2'>",
                    "<set copies='1'><is loc='Φ.mill.a🌵2-4'/></set>",
                    "<set copies='1'><is loc='Φ.mill.a🌵5-4'/></set></o></answers>"
                )
            ).folded("Φ.mill.sack"),
            Matchers.contains("[∅]")
        );
    }

    @Test
    void showsAPlaceItsOwnLocator() {
        MatcherAssert.assertThat(
            "a formation must keep its own locator in its own list, but it was hidden",
            ShapesTest.shapes("<object><o loc='Φ.mill.α0' line='3'/></object>", "<answers/>")
                .hidden("Φ.mill.α0", "Φ.mill.α0"),
            Matchers.is(false)
        );
    }

    @Test
    void hidesAFormationWithNoNameOfItsOwn() {
        MatcherAssert.assertThat(
            "a formation the parser named must be shown by its shape, but it wasnt",
            ShapesTest.shapes("<object><o loc='Φ.mill.n🌵9-2' line='9'/></object>", "<answers/>")
                .hidden("Φ.mill.n🌵9-2", "Φ.mill.sack"),
            Matchers.is(true)
        );
    }

    @Test
    void showsAFormationWithANameByItsLocator() {
        MatcherAssert.assertThat(
            "a formation with a name of its own must be shown by its locator, but it wasnt",
            ShapesTest.shapes("<object><o loc='Φ.mill' name='mill' line='1'/></object>", "<answers/>")
                .hidden("Φ.mill", "Φ.mill.sack"),
            Matchers.is(false)
        );
    }

    private static Shapes shapes(final String xmir, final String answers) {
        final Map<String, Xnav> found = new HashMap<>(0);
        final Map<String, String> files = new HashMap<>(0);
        ShapesTest.formations(new Xnav(xmir).element("object"), found, files);
        return new Shapes(found, files, new Table(new Xnav(answers)));
    }

    private static void formations(
        final Xnav node, final Map<String, Xnav> found, final Map<String, String> files
    ) {
        node.elements(Filter.withName("o")).forEach(
            kid -> {
                if (kid.attribute("base").text().isEmpty()) {
                    found.put(new Noted(kid).says("loc"), kid);
                    files.put(new Noted(kid).says("loc"), "mill.eo");
                }
                ShapesTest.formations(kid, found, files);
            }
        );
    }
}
