/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import com.jcabi.matchers.XhtmlMatchers;
import com.jcabi.xml.XMLDocument;
import com.yegor256.xsline.Xsline;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

/**
 * Test case for {@link StHex}.
 *
 * @since 0.54.0
 */
final class StHexTest {
    @ParameterizedTest
    @CsvSource({
        "1, 3F-F0-00-00-00-00-00-00",
        "2, 40-00-00-00-00-00-00-00",
        "3, 40-08-00-00-00-00-00-00",
        "10, 40-24-00-00-00-00-00-00"
    })
    void convertsNumberToHex(final String number, final String bytes) {
        MatcherAssert.assertThat(
            String.format(
                "StHex must convert number '%s' to '%s' bytes and remove @hex attribute",
                number, bytes
            ),
            new Xsline(new StHex()).pass(
                new XMLDocument(
                    String.format(
                        "<p><o base='Φ.org.eolang.number'><o base='Φ.org.eolang.bytes'><o hex=''>%s</o></o></o></p>",
                        number
                    )
                )
            ),
            Matchers.allOf(
                XhtmlMatchers.hasXPath(
                    String.format("//o[@base='Φ.org.eolang.bytes']/o[text()='%s']", bytes)
                ),
                XhtmlMatchers.hasXPath("/p[not(//o[@hex])]")
            )
        );
    }
}
