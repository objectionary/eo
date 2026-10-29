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
                "<object><listing>[] &gt; app\n  \"a b\" &gt; s\n</listing></object>",
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
                "<object><listing>[] &gt; app\n  \"a b\" &gt; s\n</listing></object>",
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
                "<object><listing>[] &gt; tea&#13;\n  [] &gt; cup&#13;\n</listing></object>",
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
                    "<object><listing>[] &gt; cup\n  [] &gt; lid\n</listing>",
                    "<o line='1' loc='Φ.cup' name='cup' pos='0'>",
                    "<o line='2' loc='Φ.cup.lid' name='lid' pos='2'/></o></object>"
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
                    "<object><listing>[] &gt; cup\n  2A-\n</listing>",
                    "<o line='1' loc='Φ.cup' name='cup' pos='0'>",
                    "<o line='2' loc='Φ.cup.α0' pos='2'>2A-</o></o></object>"
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
                    "<object><listing>[] &gt; cup\n</listing>",
                    "<o line='1' loc='Φ.cup' name='cup' pos='0'>",
                    "<o loc='Φ.cup.φ'/><o name='nobody'/><o line='1' loc='Φ.cup.ρ' pos='1'/>",
                    "</o></object>"
                ),
                "cup.eo"
            ),
            XhtmlMatchers.hasXPath("/page[@unreached='3' and @named='0']")
        );
    }

    @Test
    void leadsBackToTheIndexFromANestedPage() throws ParserConfigurationException {
        MatcherAssert.assertThat(
            "a page two folders deep must lead two folders up to the index, but it didnt",
            PageTest.drawn("<object/>", "kiln/fire/cup.eo"),
            XhtmlMatchers.hasXPath("/page[@root='../../' and @file='kiln/fire/cup.eo']")
        );
    }

    private static Document drawn(final String xmir, final String name)
        throws ParserConfigurationException {
        final Document page = DocumentBuilderFactory.newInstance().newDocumentBuilder()
            .newDocument();
        final Table table = new Table(new Xnav("<answers/>"));
        page.appendChild(
            new Page(
                new Xnav(xmir), name, table,
                new Shapes(new HashMap<>(0), new HashMap<>(0), table)
            ).drawn(page)
        );
        return page;
    }
}
