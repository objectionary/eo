/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2024 Objectionary.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */
package org.eolang.parser;

import com.jcabi.matchers.XhtmlMatchers;
import com.jcabi.xml.XMLDocument;
import com.yegor256.xsline.Xsline;
import org.hamcrest.MatcherAssert;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link StUnhex}.
 *
 * @since 0.29.0
 */
final class StUnhexTest {

    @Test
    void convertsIntFromHexToEo() {
        MatcherAssert.assertThat(
            EoIndentLexerTest.TO_ADD_MESSAGE,
            new Xsline(new StUnhex()).pass(
                new XMLDocument(
                    "<p><o base='number'><o base='org.eolang.bytes'>43-70-2E-4F-30-46-73-2E</o></o></p>"
                )
            ),
            XhtmlMatchers.hasXPaths("//o[text()='72872276393407200']")
        );
    }

    @Test
    void convertsMaxIntFromHexToEo() {
        MatcherAssert.assertThat(
            EoIndentLexerTest.TO_ADD_MESSAGE,
            new Xsline(new StUnhex()).pass(
                new XMLDocument(
                    "<p><o base='number'><o base='org.eolang.bytes'>FF-FF-FF-FF-FF-FF-FF-FF</o></o></p>"
                )
            ),
            XhtmlMatchers.hasXPaths("//o[text()='NaN']")
        );
    }

    @Test
    void convertsStringFromHexToEo() {
        MatcherAssert.assertThat(
            "String bytes must be converted to human readable string, but they didn't",
            new Xsline(new StUnhex()).pass(
                new XMLDocument(
                    String.join(
                        "",
                        "<p><o base='string'><o base='bytes'>41-42-0A-09</o></o>",
                        "<o base='string'><o base='bytes'>41-42</o></o></p>"
                    )
                )
            ),
            XhtmlMatchers.hasXPaths(
                "//o[text()='\"AB\\n\\t\"']",
                "//o[text()='\"AB\"']"
            )
        );
    }

    @Test
    void convertsEmptyStringFromHexToEo() {
        MatcherAssert.assertThat(
            EoIndentLexerTest.TO_ADD_MESSAGE,
            new Xsline(new StUnhex()).pass(
                new XMLDocument(
                    "<p><o base='string'><o base='bytes'/></o></p>"
                )
            ),
            XhtmlMatchers.hasXPaths(
                "//o[empty(text())]"
            )
        );
    }

    @Test
    void convertsFloatFromHexToEo() {
        MatcherAssert.assertThat(
            EoIndentLexerTest.TO_ADD_MESSAGE,
            new Xsline(new StUnhex()).pass(
                new XMLDocument(
                    "<p><o base='number'><o base='org.eolang.bytes'>41-42-43-67-AE-CD-3E-FD</o></o></p>"
                )
            ),
            XhtmlMatchers.hasXPaths(
                "//o[text()='2393807.3656386123']"
            )
        );
    }
}
