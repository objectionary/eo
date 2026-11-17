/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.github.lombrozo.xnav.Xnav;
import com.jcabi.matchers.XhtmlMatchers;
import java.util.HashMap;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.w3c.dom.Document;

/**
 * Test case for {@link Page}.
 *
 * @since 0.75.0
 */
final class PageTest {

    @Test
    void breaksTheListingWhereTheParserBreaksIt() throws ParserConfigurationException {
        MatcherAssert.assertThat(
            "a separator the parser does not count must stay inside its line, but it opened a new one",
            PageTest.drawn(
                "<object><listing>[] &gt; app&#10;  \"a b\" &gt; s&#10;</listing></object>",
                "<answers/>",
                "app.eo"
            ),
            XhtmlMatchers.hasXPath("/page[count(line)=2]")
        );
    }

    @Test
    void keepsTheSeparatorInsideTheLineItWasWrittenOn() throws ParserConfigurationException {
        MatcherAssert.assertThat(
            "the text around the separator must stay on one line, but it was cut in two",
            PageTest.drawn(
                "<object><listing>[] &gt; app&#10;  \"a b\" &gt; s&#10;</listing></object>",
                "<answers/>",
                "app.eo"
            ),
            XhtmlMatchers.hasXPath("/page/line[@n='2']/bit[contains(., 'b')]")
        );
    }

    @Test
    void breaksALineAtACarriageReturnBeforeTheFeed() throws ParserConfigurationException {
        MatcherAssert.assertThat(
            "a carriage return before a line feed must end the line with it, but it stayed",
            PageTest.drawn(
                "<object><listing>[] &gt; tea&#13;&#10;  [] &gt; cup&#13;&#10;</listing></object>",
                "<answers/>",
                "tea.eo"
            ),
            XhtmlMatchers.hasXPath("/page[count(line)=2]/line[1]/bit[.='[] > tea']")
        );
    }

    @Test
    void marksAnObjectOnTheLineItWasWrittenOn() throws ParserConfigurationException {
        MatcherAssert.assertThat(
            "an object must be marked on its own line, but it wasnt",
            PageTest.drawn(
                String.join(
                    "",
                    "<object><listing>[] &gt; cup&#10;  [] &gt; lid&#10;</listing>",
                    "<o line='1' loc='Φ.cup' name='cup' pos='0'>",
                    "<o line='2' loc='Φ.cup.lid' name='lid' pos='2'/></o></object>"
                ),
                String.join(
                    "",
                    "<answers><o loc='Φ.cup' copies='3'><is loc='Φ.cup'/></o>",
                    "<o loc='Φ.cup.lid' copies='5'><is loc='Φ.number'/></o></answers>"
                ),
                "cup.eo"
            ),
            XhtmlMatchers.hasXPath("/page/line[@n='2']/bit[text='lid']/told[@loc='Φ.cup.lid']")
        );
    }

    @Test
    void leavesTheBytesOfALiteralUnmarked() throws ParserConfigurationException {
        MatcherAssert.assertThat(
            "the bytes of a literal must get no mark of their own, but they got one",
            PageTest.drawn(
                String.join(
                    "",
                    "<object><listing>[] &gt; cup&#10;  2A-&#10;</listing>",
                    "<o line='1' loc='Φ.cup' name='cup' pos='0'>",
                    "<o line='2' loc='Φ.cup.α0' pos='2'>2A-</o></o></object>"
                ),
                String.join(
                    "",
                    "<answers><o loc='Φ.cup' copies='2'><is loc='Φ.cup'/></o>",
                    "<o loc='Φ.cup.α0' copies='2'><data/></o></answers>"
                ),
                "cup.eo"
            ),
            XhtmlMatchers.hasXPath("/page/line[@n='2'][not(bit/told)]")
        );
    }

    @Test
    void talliesEveryObjectWithALocator() throws ParserConfigurationException {
        MatcherAssert.assertThat(
            "the tally must count every object with a locator, written or not, but it didnt",
            PageTest.drawn(
                String.join(
                    "",
                    "<object><listing>[] &gt; cup&#10;</listing>",
                    "<o line='1' loc='Φ.cup' name='cup' pos='0'>",
                    "<o loc='Φ.cup.φ'/><o name='nobody'/><o line='1' loc='Φ.cup.ρ' pos='1'/>",
                    "</o></object>"
                ),
                String.join(
                    "",
                    "<answers><o loc='Φ.cup' copies='4'><is loc='Φ.cup'/></o>",
                    "<o loc='Φ.cup.φ' copies='4'><is loc='Φ.bytes'/></o>",
                    "<o loc='Φ.cup.ρ' copies='4'><is loc='Φ.tea'/></o></answers>"
                ),
                "cup.eo"
            ),
            XhtmlMatchers.hasXPath("/page[@named='3']")
        );
    }

    @Test
    void refusesToDrawAnObjectTheAnswersSayNothingOf() {
        MatcherAssert.assertThat(
            "a page must name the object the answers say nothing of, but it didnt",
            Assertions.assertThrows(
                IllegalStateException.class,
                () -> PageTest.drawn(
                    String.join(
                        "",
                        "<object><listing>[] &gt; jug&#10;  [] &gt; spout&#10;</listing>",
                        "<o line='1' loc='Φ.jug' name='jug' pos='0'>",
                        "<o line='2' loc='Φ.jug.spout' name='spout' pos='2'/></o></object>"
                    ),
                    "<answers><o loc='Φ.jug' copies='6'><is loc='Φ.jug'/></o></answers>",
                    "jug.eo"
                ),
                "a page must refuse an object the answers say nothing of"
            ).getMessage(),
            Matchers.containsString("Φ.jug.spout")
        );
    }

    @Test
    void leadsBackToTheIndexFromANestedPage() throws ParserConfigurationException {
        MatcherAssert.assertThat(
            "a page two folders deep must lead two folders up to the index, but it didnt",
            PageTest.drawn("<object/>", "<answers/>", "kiln/fire/cup.eo"),
            XhtmlMatchers.hasXPath("/page[@root='../../' and @file='kiln/fire/cup.eo']")
        );
    }

    private static Document drawn(final String xmir, final String answers, final String name)
        throws ParserConfigurationException {
        final Document page = DocumentBuilderFactory.newInstance().newDocumentBuilder()
            .newDocument();
        final Table table = new Table(new Xnav(answers));
        page.appendChild(
            new Page(
                new Xnav(xmir), name, table,
                new Shapes(new HashMap<>(0), new HashMap<>(0), table)
            ).drawn(page)
        );
        return page;
    }
}
