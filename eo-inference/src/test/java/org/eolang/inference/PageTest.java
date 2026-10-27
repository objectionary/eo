/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.jcabi.matchers.XhtmlMatchers;
import com.jcabi.xml.XMLDocument;
import java.util.HashMap;
import org.hamcrest.MatcherAssert;
import org.junit.jupiter.api.Test;
import org.xembly.Directives;
import org.xembly.Xembler;

/**
 * Test case for {@link Page}.
 *
 * @since 0.75.0
 */
final class PageTest {

    @Test
    void breaksTheListingWhereTheParserBreaksIt() {
        MatcherAssert.assertThat(
            "a separator the parser does not count must stay inside its line, but it opened a new one",
            PageTest.drawn(),
            XhtmlMatchers.hasXPath("/page[count(line) = 2]")
        );
    }

    @Test
    void keepsTheSeparatorInsideTheLineItWasWrittenOn() {
        MatcherAssert.assertThat(
            "the text around the separator must stay on one line, but it was cut in two",
            PageTest.drawn(),
            XhtmlMatchers.hasXPath("/page/line[@n = '2']/bit[contains(., 'b')]")
        );
    }

    private static String drawn() {
        return new Xembler(
            new Page(
                new XMLDocument(
                    new Xembler(
                        new Directives().add("object").add("listing").set(
                            String.format(
                                "[] > app%n  \"a%sb\" > s%n", Character.toString(0x2028)
                            )
                        )
                    ).xmlQuietly()
                ),
                new HashMap<>(0)
            ).directives("app.eo")
        ).xmlQuietly();
    }
}
