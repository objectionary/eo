/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
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
    void convertsNumberFromHexToEo(final Shift shift, final String type) {
        MatcherAssert.assertThat(
            String.format("StUnhex by %s must successfully unhex number", type),
            new Xsline(new StUnhex(shift)).pass(
                new XMLDocument(
                    "<p><o base='Φ.org.eolang.number'><o base='Φ.org.eolang.bytes'><o>43-70-2E-4F-30-46-73-2E</o></o></o></p>"
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
    void convertsMaxIntFromHexToEo(final Shift shift, final String type) {
        MatcherAssert.assertThat(
            String.format("StUnhex by %s must skip unhexing max integer number", type),
            new Xsline(new StUnhex(shift)).pass(
                new XMLDocument(
                    "<p><o base='Φ.org.eolang.number'><o base='Φ.org.eolang.bytes'><o>FF-FF-FF-FF-FF-FF-FF-FF</o></o></o></p>"
                )
            ),
            XhtmlMatchers.hasXPath(
                "//o[@base='Φ.org.eolang.number' and o[@base='Φ.org.eolang.bytes' and not(o) and text()!='']]"
            )
        );
    }

    @ParameterizedTest
    @MethodSource("shifts")
    void convertsStringFromHexToEo(final Shift shift, final String type) {
        MatcherAssert.assertThat(
            String.format(
                "StUnhex with %s must unhex bytes to human readable string, but they didn't",
                type
            ),
            new Xsline(new StUnhex(shift)).pass(
                new XMLDocument(
                    String.join(
                        "",
                        "<p><o base='Φ.org.eolang.string'><o base='Φ.org.eolang.bytes'><o>41-42-0A-09</o></o></o>",
                        "<o base='Φ.org.eolang.string'><o base='Φ.org.eolang.bytes'><o>41-42</o></o></o></p>"
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
    void convertsEmptyStringFromHexToEo(final Shift shift, final String type) {
        MatcherAssert.assertThat(
            String.format("StUnhex by %s must convert empty string", type),
            new Xsline(new StUnhex(shift)).pass(
                new XMLDocument(
                    "<p><o base='Φ.org.eolang.string'><o base='Φ.org.eolang.bytes'/><o/></o></p>"
                )
            ),
            XhtmlMatchers.hasXPath("//o[empty(text())]")
        );
    }

    @ParameterizedTest
    @MethodSource("shifts")
    void convertsStringWithDoubleSpacesFromHexToEo(final Shift shift, final String type) {
        MatcherAssert.assertThat(
            String.format("StUnhex by %s must convert string with double spaces", type),
            new Xsline(new StUnhex(shift)).pass(
                new XMLDocument(
                    "<o base=\"Φ.org.eolang.string\"><o base=\"Φ.org.eolang.bytes\"><o>7A-0A-20-20-79-0A-20-78</o></o></o>"
                )
            ),
            XhtmlMatchers.hasXPath("//o[text()='\"z\\n  y\\n x\"']")
        );
    }

    @ParameterizedTest
    @MethodSource("shifts")
    void convertsNegativeZeroFromHexToEo(final Shift shift, final String type) {
        MatcherAssert.assertThat(
            String.format("StUnhex by %s must convert negative zero", type),
            new Xsline(new StUnhex(shift)).pass(
                new XMLDocument(
                    "<o base='Φ.org.eolang.number'><o base='Φ.org.eolang.bytes'><o>80-00-00-00-00-00-00-00</o></o></o>"
                )
            ),
            XhtmlMatchers.hasXPath("//o[text()='-0']")
        );
    }

    @ParameterizedTest
    @MethodSource("shifts")
    void convertsFloatFromHexToEo(final Shift shift, final String type) {
        MatcherAssert.assertThat(
            String.format("StUnhex by %s must convert float", type),
            new Xsline(new StUnhex(shift)).pass(
                new XMLDocument(
                    "<p><o base='Φ.org.eolang.number'><o base='Φ.org.eolang.bytes'><o>41-42-43-67-AE-CD-3E-FD</o></o></o></p>"
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
            Arguments.of(StUnhex.XNAV, "xnav")
        );
    }
}
