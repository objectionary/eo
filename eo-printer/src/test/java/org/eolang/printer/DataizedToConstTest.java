/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.printer;

import com.jcabi.matchers.XhtmlMatchers;
import com.jcabi.xml.XMLDocument;
import com.yegor256.xsline.StClasspath;
import com.yegor256.xsline.Xsline;
import org.hamcrest.MatcherAssert;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@code dataized-to-const.xsl}.
 * @since 0.62.0
 */
final class DataizedToConstTest {

    @Test
    void foldsAPayloadOnlyLiteralIntoAConst() {
        MatcherAssert.assertThat(
            "an argument whose own text is its whole payload, with no nested value, must fold into a const carrying that same text",
            new Xsline(new StClasspath("/org/eolang/printer/print/dataized-to-const.xsl")).pass(
                new XMLDocument(
                    "<p><o base='.as-bytes'><o base='Φ.dataized'><o base='Φ.bytes'>2A-</o></o></o></p>"
                )
            ),
            XhtmlMatchers.hasXPath("//o[@base='Φ.bytes' and @const and text()='2A-']")
        );
    }

    @Test
    void foldsALiteralWhoseOwnPayloadIsNested() {
        MatcherAssert.assertThat(
            "an argument with no direct text of its own, only a nested value child (the everyday shape for a number literal), must fold into a const with that child untouched and no direct text of its own",
            new Xsline(new StClasspath("/org/eolang/printer/print/dataized-to-const.xsl")).pass(
                new XMLDocument(
                    String.join(
                        "",
                        "<p><o base='.as-bytes'><o base='Φ.dataized'>",
                        "<o base='Φ.number'><o base='Φ.bytes'>2A-</o></o>",
                        "</o></o></p>"
                    )
                )
            ),
            XhtmlMatchers.hasXPath(
                "//o[@base='Φ.number' and @const and not(text()[normalize-space()]) and o[@base='Φ.bytes' and text()='2A-']]"
            )
        );
    }

    @Test
    void copiesTheOriginalUnchangedWhenNoArgumentExists() {
        MatcherAssert.assertThat(
            "a Φ.dataized wrapper with nothing to fold must be copied through untouched",
            new Xsline(new StClasspath("/org/eolang/printer/print/dataized-to-const.xsl")).pass(
                new XMLDocument(
                    "<p><o base='.as-bytes'><o base='Φ.dataized'/></o></p>"
                )
            ),
            XhtmlMatchers.hasXPath("//o[@base='.as-bytes']/o[@base='Φ.dataized' and not(node())]")
        );
    }

    @Test
    void keepsLiteralPayloadApartFromArgument() {
        MatcherAssert.assertThat(
            "the payload of a data literal must not be glued to the text of its argument - this fragment is the shape #5721 would produce, not one the current pipeline can build yet (#6102)",
            new Xsline(new StClasspath("/org/eolang/printer/print/dataized-to-const.xsl")).pass(
                new XMLDocument(
                    String.join(
                        "",
                        "<p><o base='.as-bytes' name='x'><o base='Φ.dataized'>",
                        "<o base='Φ.bytes'>01-02<o base='Φ.number'>5</o></o>",
                        "</o></o></p>"
                    )
                )
            ),
            XhtmlMatchers.hasXPath(
                "//o[@base='Φ.bytes' and @const and text()='01-02' and o[text()='5']]"
            )
        );
    }
}
