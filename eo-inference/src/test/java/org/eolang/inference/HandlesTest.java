/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.jcabi.matchers.XhtmlMatchers;
import java.util.Map;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import org.hamcrest.MatcherAssert;
import org.junit.jupiter.api.Test;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 * Test case for {@link Handles}.
 *
 * @since 0.76.0
 */
final class HandlesTest {

    @Test
    void spellsAMadeSegmentByTheNameItsAuthorGaveIt() throws ParserConfigurationException {
        MatcherAssert.assertThat(
            "a segment the parser made must be spelled by the name the author gave, but it wasnt",
            HandlesTest.spelled(Map.of("Φ.oven.a🌵7-3", "hatch"), "Φ.oven.a🌵7-3.door"),
            XhtmlMatchers.hasXPath(
                "/page/said/name[@loc='Φ.oven.a🌵7-3.door']/handle[@hidden='a🌵7-3' and .='hatch']"
            )
        );
    }

    @Test
    void keepsTheWordsAroundANameInAShape() throws ParserConfigurationException {
        MatcherAssert.assertThat(
            "the words around a spelled name must stay as they were, but they didnt",
            HandlesTest.spelled(
                Map.of("Φ.oven.a🌵7-3", "hatch"), "[^ ∅] → Φ.oven.a🌵7-3.door or Φ.bool"
            ),
            XhtmlMatchers.hasXPath("/page/said[.='[^ ∅] → Φ.oven.hatch.door or Φ.bool']")
        );
    }

    @Test
    void leavesAVoidItsAuthorNamedAsTheLocatorSaysIt() throws ParserConfigurationException {
        MatcherAssert.assertThat(
            "a void the author named must keep its locator, since the parser made no name, but it didnt",
            HandlesTest.spelled(Map.of("Φ.oven.ρ", "stove"), "Φ.oven.ρ.heat"),
            XhtmlMatchers.hasXPath("/page/said[not(name) and .='Φ.oven.ρ.heat']")
        );
    }

    private static Document spelled(final Map<String, String> names, final String words)
        throws ParserConfigurationException {
        final Document page = DocumentBuilderFactory.newInstance().newDocumentBuilder()
            .newDocument();
        final Element root = page.createElement("page");
        final Element said = page.createElement("said");
        said.appendChild(page.createTextNode(words));
        root.appendChild(said);
        page.appendChild(root);
        new Handles(names).spelled(root);
        return page;
    }
}
