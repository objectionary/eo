/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import com.jcabi.matchers.XhtmlMatchers;
import com.jcabi.xml.XMLDocument;
import com.yegor256.xsline.StEndless;
import com.yegor256.xsline.Xsline;
import org.hamcrest.MatcherAssert;
import org.junit.jupiter.api.Test;
import org.xembly.Directives;

/**
 * Test case for {@link StXPath}.
 *
 * @since 0.29.0
 */
final class StXPathTest {

    @Test
    void modifiesSimpleNode() {
        MatcherAssert.assertThat(
            EoIndentLexerTest.TO_ADD_MESSAGE,
            new Xsline(
                new StEndless(
                    new StXPath(
                        "//x[@a and not(@b)]",
                        xml -> new Directives().attr(
                            "b", xml.xpath("text()").get(0)
                        )
                    )
                )
            ).pass(new XMLDocument("<p><x a='1'>foo</x><x><x a='2'>bar</x></x></p>")),
            XhtmlMatchers.hasXPaths(
                "//x[@b='foo']",
                "//x[@b='bar']"
            )
        );
    }

}
