/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.github.lombrozo.xnav.Xnav;
import com.jcabi.matchers.XhtmlMatchers;
import java.util.Arrays;
import java.util.HashMap;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import org.hamcrest.MatcherAssert;
import org.junit.jupiter.api.Test;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 * Test case for {@link Pieces}.
 *
 * @since 0.70.0
 */
final class PiecesTest {

    @Test
    void walksAChainOfReceiversLeftward() throws ParserConfigurationException {
        MatcherAssert.assertThat(
            "each link of the chain must get its own word, but they piled onto one",
            PiecesTest.drawn(
                "  first.as-bytes.size",
                new Written("Φ.f.d", 16, "", ".size"),
                new Written("Φ.f.d.ρ", 16, "", ".as-bytes"),
                new Written("Φ.f.d.ρ.ρ", 16, "", ".first")
            ),
            XhtmlMatchers.hasXPaths(
                "/line/bit[text='first']/told[@loc='Φ.f.d.ρ.ρ']",
                "/line/bit[text='.as-bytes']/told[@loc='Φ.f.d.ρ']",
                "/line/bit[text='.size']/told[@loc='Φ.f.d']"
            )
        );
    }

    @Test
    void marksTheCaretAChainIsDispatchedOn() throws ParserConfigurationException {
        MatcherAssert.assertThat(
            "the caret a walk is taken off must get a word of its own, but it stayed bare",
            PiecesTest.drawn(
                "  ^.walk",
                new Written("Φ.w.α1", 3, "", ".walk"),
                new Written("Φ.w.α1.ρ", 3, "ρ", "ξ.ρ")
            ),
            XhtmlMatchers.hasXPaths(
                "/line/bit[text='^']/told[@loc='Φ.w.α1.ρ']",
                "/line/bit[text='.walk']/told[@loc='Φ.w.α1']"
            )
        );
    }

    @Test
    void marksADispatchTakenOffTheCaret() throws ParserConfigurationException {
        MatcherAssert.assertThat(
            "the step .^ must carry its dot into the mark, but the dot was marked alone",
            PiecesTest.drawn(
                "* ^.^",
                new Written("Φ.t.α1", 3, "", ".ρ"),
                new Written("Φ.t.α1.ρ", 3, "ρ", "ξ.ρ")
            ),
            XhtmlMatchers.hasXPath("/line/bit[text='.^']/told[@loc='Φ.t.α1']")
        );
    }

    @Test
    void keepsTextThatNoObjectClaims() throws ParserConfigurationException {
        MatcherAssert.assertThat(
            "the brackets around a void are the author's text and must survive, but they didnt",
            PiecesTest.drawn("[if] > bool", new Written("Φ.bool.if", 1, "if", "∅")),
            XhtmlMatchers.hasXPaths(
                "/line/bit[1][.='[' and not(@band)]",
                "/line/bit[2][text='if']",
                "/line/bit[3][.='] > bool' and not(@band)]"
            )
        );
    }

    @Test
    void marksTheNameAFormationIsBoundTo() throws ParserConfigurationException {
        MatcherAssert.assertThat(
            "a formation must be marked on its name, but the bracket that opens it was marked",
            PiecesTest.drawn("[x] > foo", new Written("Φ.foo", 0, "foo", "")),
            XhtmlMatchers.hasXPaths(
                "/line/bit[1][.='[x] > ' and not(@band)]",
                "/line/bit[2][text='foo']/told[@loc='Φ.foo']"
            )
        );
    }

    @Test
    void marksTheNameAFormationIsBoundToTwice() throws ParserConfigurationException {
        MatcherAssert.assertThat(
            "a formation behind a double arrow must be marked on its name, but it wasnt",
            PiecesTest.drawn("  [ü] >> ñame", new Written("Φ.ñame", 2, "ñame", "")),
            XhtmlMatchers.hasXPath("/line/bit[text='ñame']/told")
        );
    }

    @Test
    void keepsTheBracketOfAFormationBoundToNoName() throws ParserConfigurationException {
        MatcherAssert.assertThat(
            "a formation with no name must stay marked on its bracket, but it vanished",
            PiecesTest.drawn("  seq * > [^ m] >>", new Written("Φ.f.φ.α1", 10, "", "")),
            XhtmlMatchers.hasXPath("/line/bit[text='[']/told[@loc='Φ.f.φ.α1']")
        );
    }

    @Test
    void tellsTheOutermostOfTheObjectsWrittenOnce() throws ParserConfigurationException {
        MatcherAssert.assertThat(
            "a literal and its bytes must be told by the literal, but the bytes were",
            PiecesTest.drawn(
                "  plus 42",
                new Written("Φ.p.α0", 7, "", "Φ.number"),
                new Written("Φ.p.α0.α0", 7, "", "Φ.bytes")
            ),
            XhtmlMatchers.hasXPath("/line/bit[text='42'][count(told)=1]/told[@loc='Φ.p.α0']")
        );
    }

    @Test
    void tellsTheObjectWhoseBaseIsTheWord() throws ParserConfigurationException {
        MatcherAssert.assertThat(
            "the read the source spells must be told, but the object around it was",
            PiecesTest.drawn(
                "  lid > cap",
                new Written("Φ.jar.cap", 2, "cap", "Φ.box"),
                new Written("Φ.jar.cap.ρ.α0", 2, "", ".lid")
            ),
            XhtmlMatchers.hasXPath("/line/bit[text='lid']/told[@loc='Φ.jar.cap.ρ.α0']")
        );
    }

    @Test
    void tellsTheObjectNamedByTheWord() throws ParserConfigurationException {
        MatcherAssert.assertThat(
            "the object whose name is the word must be told, but another one was",
            PiecesTest.drawn(
                "  lid",
                new Written("Φ.jar", 2, "", "Φ.box"),
                new Written("Φ.jar.φ.lid", 2, "lid", "Φ.cap")
            ),
            XhtmlMatchers.hasXPath("/line/bit[text='lid']/told[@loc='Φ.jar.φ.lid']")
        );
    }

    @Test
    void readsTheCaretAsTheReceiverItStandsFor() throws ParserConfigurationException {
        MatcherAssert.assertThat(
            "a caret must be read as ρ when the object is chosen, but it wasnt",
            PiecesTest.drawn(
                "  ^",
                new Written("Φ.k.α0", 2, "", "Φ.x"),
                new Written("Φ.k.α0.φ.ρ", 2, "", "ρ")
            ),
            XhtmlMatchers.hasXPath("/line/bit[text='^']/told[@loc='Φ.k.α0.φ.ρ']")
        );
    }

    @Test
    void leavesALineBareWhenItsObjectsLieBeyondIt() throws ParserConfigurationException {
        MatcherAssert.assertThat(
            "an object past the end of the line must not be marked, but it was",
            PiecesTest.drawn("  oak", new Written("Φ.grove.oak", 40, "oak", "")),
            XhtmlMatchers.hasXPath("/line[count(bit)=1]/bit[.='  oak' and not(told)]")
        );
    }

    private static Document drawn(final String line, final Written... objects)
        throws ParserConfigurationException {
        final Document page = DocumentBuilderFactory.newInstance().newDocumentBuilder()
            .newDocument();
        final Element row = page.createElement("line");
        page.appendChild(row);
        final Table table = new Table(new Xnav("<answers/>"));
        new Pieces(line, Arrays.asList(objects)).drawn(
            row,
            new Entries(table, new Shapes(new HashMap<>(0), new HashMap<>(0), table), "app.eo")
        );
        return page;
    }
}
