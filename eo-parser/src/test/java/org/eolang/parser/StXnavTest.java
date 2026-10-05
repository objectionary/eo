/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import com.jcabi.matchers.XhtmlMatchers;
import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link StXnav}.
 *
 * @since 0.53.0
 */
final class StXnavTest {

    @Test
    void leavesTheGivenXmlAlone() {
        final XML before = new XMLDocument("<p><o base='a'>text</o></p>");
        final String snapshot = before.toString();
        new StXnav("/p/o", node -> node.node().setTextContent("changed")).apply(0, before);
        MatcherAssert.assertThat(
            "the shift must not touch the document it was given, only the one it returns",
            before.toString(),
            Matchers.equalTo(snapshot)
        );
    }

    @Test
    void appliesTheFunctionToTheResult() {
        MatcherAssert.assertThat(
            "the returned document must carry what the function did",
            new StXnav(
                "/p/o",
                node -> node.node().setTextContent("changed")
            ).apply(0, new XMLDocument("<p><o base='a'>text</o></p>")),
            XhtmlMatchers.hasXPath("/p/o[text()='changed']")
        );
    }
}
