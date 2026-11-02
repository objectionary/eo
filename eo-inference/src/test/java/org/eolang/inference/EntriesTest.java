/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.github.lombrozo.xnav.Filter;
import com.github.lombrozo.xnav.Xnav;
import com.jcabi.matchers.XhtmlMatchers;
import java.util.HashMap;
import java.util.Map;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import org.hamcrest.MatcherAssert;
import org.junit.jupiter.api.Test;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 * Test case for {@link Entries}.
 *
 * @since 0.76.0
 */
final class EntriesTest {

    @Test
    void coloursAMarkByItsBand() throws ParserConfigurationException {
        MatcherAssert.assertThat(
            "a mark over one name must be green, but it wasnt",
            EntriesTest.told(
                "<answers><o loc='Φ.kiln.heat' copies='3'><is loc='Φ.number'/></o></answers>",
                "kiln.eo",
                "Φ.kiln.heat"
            ),
            XhtmlMatchers.hasXPath("/bit[@band='named']")
        );
    }

    @Test
    void countsTheCopiesOfThePlace() throws ParserConfigurationException {
        MatcherAssert.assertThat(
            "the mark must say how many copies were walked, but it didnt",
            EntriesTest.told(
                String.join(
                    "",
                    "<answers><o loc='Φ.kiln.heat' copies='232'>",
                    "<set copies='119'><terminator/><is loc='Φ.bool'/></set>",
                    "<set copies='113'><is loc='Φ.bool'/></set></o></answers>"
                ),
                "kiln.eo",
                "Φ.kiln.heat"
            ),
            XhtmlMatchers.hasXPath("/bit/told[@loc='Φ.kiln.heat' and @copies='232']")
        );
    }

    @Test
    void listsTheAnswerWithTheMostCopiesFirst() throws ParserConfigurationException {
        MatcherAssert.assertThat(
            "the answer most copies give must come first, but it didnt",
            EntriesTest.told(
                String.join(
                    "",
                    "<answers><o loc='Φ.kiln.heat' copies='232'>",
                    "<set copies='119'><terminator/><is loc='Φ.bool'/></set>",
                    "<set copies='113'><is loc='Φ.bool'/></set></o></answers>"
                ),
                "kiln.eo",
                "Φ.kiln.heat"
            ),
            XhtmlMatchers.hasXPaths(
                "/bit/told/said[1][@kind='name' and @n='232' and .='Φ.bool']",
                "/bit/told/said[2][@n='119']"
            )
        );
    }

    @Test
    void saysAMarkOfTheEngineInWords() throws ParserConfigurationException {
        MatcherAssert.assertThat(
            "an error must be said in words, but it wasnt",
            EntriesTest.told(
                "<answers><o loc='Φ.kiln.ash' copies='2'><terminator/></o></answers>",
                "kiln.eo",
                "Φ.kiln.ash"
            ),
            XhtmlMatchers.hasXPath("/bit/told/said[@kind='words' and .='an error (T)']")
        );
    }

    @Test
    void joinsFormationsOfOneShapeIntoOneEntry() throws ParserConfigurationException {
        MatcherAssert.assertThat(
            "two formations of one shape must be one entry, but they were two",
            EntriesTest.told(EntriesTest.twins(), "mill.eo", "Φ.mill.sack"),
            XhtmlMatchers.hasXPath("/bit/told[count(said)=2]/said[@kind='object' and @n='6']")
        );
    }

    @Test
    void linksEveryLineWhereAShapeIsWritten() throws ParserConfigurationException {
        MatcherAssert.assertThat(
            "a shape must link every line where one of its formations is written, but it didnt",
            EntriesTest.told(EntriesTest.twins(), "mill.eo", "Φ.mill.sack"),
            XhtmlMatchers.hasXPaths(
                "/bit/told/said[@kind='object']/at[1][@lead='lines ' and @href='#L2' and .='2']",
                "/bit/told/said[@kind='object']/at[2][@lead='' and @href='#L5' and .='5']"
            )
        );
    }

    @Test
    void namesTheFormationALinkLeadsTo() throws ParserConfigurationException {
        MatcherAssert.assertThat(
            "a link must carry the locator of its formation, but it didnt",
            EntriesTest.told(EntriesTest.twins(), "mill.eo", "Φ.mill.sack"),
            XhtmlMatchers.hasXPath("/bit/told/said/at[@loc='Φ.mill.a🌵5-4']")
        );
    }

    @Test
    void linksAShapeWrittenInAnotherFile() throws ParserConfigurationException {
        MatcherAssert.assertThat(
            "a link into another file must lead there, under the name of that file, but it didnt",
            EntriesTest.told(EntriesTest.twins(), "deep/kiln.eo", "Φ.mill.sack"),
            XhtmlMatchers.hasXPath(
                "/bit/told/said/at[1][@lead='mill.eo:' and @href='../mill.eo.html#L2']"
            )
        );
    }

    private static String twins() {
        return String.join(
            "",
            "<answers><o loc='Φ.mill.sack' copies='9'>",
            "<set copies='4'><is loc='Φ.mill.a🌵5-4'/></set>",
            "<set copies='3'><terminator/></set>",
            "<set copies='2'><is loc='Φ.mill.a🌵2-4'/><terminator/></set>",
            "</o></answers>"
        );
    }

    private static Document told(final String answers, final String name, final String loc)
        throws ParserConfigurationException {
        final Map<String, Xnav> found = new HashMap<>(0);
        final Map<String, String> files = new HashMap<>(0);
        new Xnav(
            String.join(
                "",
                "<object>",
                "<o loc='Φ.mill.a🌵2-4' line='2'><o base='∅' name='x' loc='Φ.mill.a🌵2-4.x'/></o>",
                "<o loc='Φ.mill.a🌵5-4' line='5'><o base='∅' name='y' loc='Φ.mill.a🌵5-4.y'/></o>",
                "</object>"
            )
        ).element("object").elements(Filter.withName("o")).forEach(
            formation -> {
                found.put(new Noted(formation).says("loc"), formation);
                files.put(new Noted(formation).says("loc"), "mill.eo");
            }
        );
        final Table table = new Table(new Xnav(answers));
        final Document page = DocumentBuilderFactory.newInstance().newDocumentBuilder()
            .newDocument();
        final Element bit = page.createElement("bit");
        page.appendChild(bit);
        new Entries(table, new Shapes(found, files, table), name).told(bit, loc);
        return page;
    }
}
