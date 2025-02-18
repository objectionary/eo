/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2025 Objectionary.com
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
import com.yegor256.xsline.Shift;
import com.yegor256.xsline.Xsline;
import java.util.stream.Stream;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

/**
 * Test case for {@link StUnhex}.
 *
 * @since 0.29.0
 */
final class StUnhexTest {

    @ParameterizedTest
    @MethodSource("shifts")
    void convertsIntFromHexToEo(final Shift shift) {
        MatcherAssert.assertThat(
            EoIndentLexerTest.TO_ADD_MESSAGE,
            new Xsline(new StUnhex(shift)).pass(
                new XMLDocument(
                    "<p><o base='Q.org.eolang.number'><o base='Q.org.eolang.bytes'>43-70-2E-4F-30-46-73-2E</o></o></p>"
                )
            ),
            Matchers.anyOf(
                XhtmlMatchers.hasXPath("//o[text()='72872276393407200']"),
                XhtmlMatchers.hasXPath("//o[text()='7.28722763934072e16']")
            )
        );
    }

    @ParameterizedTest
    @MethodSource("shifts")
    void convertsMaxIntFromHexToEo(final Shift shift) {
        MatcherAssert.assertThat(
            EoIndentLexerTest.TO_ADD_MESSAGE,
            new Xsline(new StUnhex(shift)).pass(
                new XMLDocument(
                    "<p><o base='Q.org.eolang.number'><o base='Q.org.eolang.bytes'>FF-FF-FF-FF-FF-FF-FF-FF</o></o></p>"
                )
            ),
            XhtmlMatchers.hasXPath(
                "//o[@base='Q.org.eolang.number' and o[@base='Q.org.eolang.bytes' and text()!='']]"
            )
        );
    }

    @ParameterizedTest
    @MethodSource("shifts")
    void convertsStringFromHexToEo(final Shift shift) {
        MatcherAssert.assertThat(
            "String bytes must be converted to human readable string, but they didn't",
            new Xsline(new StUnhex(shift)).pass(
                new XMLDocument(
                    String.join(
                        "",
                        "<p><o base='Q.org.eolang.string'><o base='Q.org.eolang.bytes'>41-42-0A-09</o></o>",
                        "<o base='Q.org.eolang.string'><o base='Q.org.eolang.bytes'>41-42</o></o></p>"
                    )
                )
            ),
            XhtmlMatchers.hasXPaths(
                "//o[text()='\"AB\\n\\t\"']",
                "//o[text()='\"AB\"']"
            )
        );
    }

    @ParameterizedTest
    @MethodSource("shifts")
    void convertsEmptyStringFromHexToEo(final Shift shift) {
        MatcherAssert.assertThat(
            EoIndentLexerTest.TO_ADD_MESSAGE,
            new Xsline(new StUnhex(shift)).pass(
                new XMLDocument(
                    "<p><o base='Q.org.eolang.string'><o base='Q.org.eolang.bytes'/></o></p>"
                )
            ),
            XhtmlMatchers.hasXPath("//o[empty(text())]")
        );
    }

    @ParameterizedTest
    @MethodSource("shifts")
    void convertsStringWithDoubleSpacesFromHexToEo(final Shift shift) {
        MatcherAssert.assertThat(
            EoIndentLexerTest.TO_ADD_MESSAGE,
            new Xsline(new StUnhex(shift)).pass(
                new XMLDocument(
                    "<o base=\"Q.org.eolang.string\"><o base=\"Q.org.eolang.bytes\">7A-0A-20-20-79-0A-20-78</o></o>"
                )
            ),
            XhtmlMatchers.hasXPath("//o[text()='\"z\\n  y\\n x\"']")
        );
    }

    @ParameterizedTest
    @MethodSource("shifts")
    void convertsNegativeZeroFromHexToEo(final Shift shift) {
        MatcherAssert.assertThat(
            EoIndentLexerTest.TO_ADD_MESSAGE,
            new Xsline(new StUnhex(shift)).pass(
                new XMLDocument(
                    "<o base='Q.org.eolang.number'><o base='Q.org.eolang.bytes'>80-00-00-00-00-00-00-00</o></o>"
                )
            ),
            XhtmlMatchers.hasXPath("//o[text()='-0']")
        );
    }

    @ParameterizedTest
    @MethodSource("shifts")
    void convertsFloatFromHexToEo(final Shift shift) {
        MatcherAssert.assertThat(
            EoIndentLexerTest.TO_ADD_MESSAGE,
            new Xsline(new StUnhex(shift)).pass(
                new XMLDocument(
                    "<p><o base='Q.org.eolang.number'><o base='Q.org.eolang.bytes'>41-42-43-67-AE-CD-3E-FD</o></o></p>"
                )
            ),
            Matchers.anyOf(
                XhtmlMatchers.hasXPath("//o[text()='2393807.3656386123']"),
                XhtmlMatchers.hasXPath("//o[text()='2.3938073656386123e6']")
            )
        );
    }

    @SuppressWarnings("PMD.UnusedPrivateMethod")
    private static Stream<Arguments> shifts() {
        return Stream.of(
            Arguments.of(StUnhex.XNAV),
            Arguments.of(StUnhex.XPATH),
            Arguments.of(StUnhex.XSL)
        );
    }
}
