/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.printer;

import com.jcabi.xml.XMLDocument;
import com.yegor256.xsline.StClasspath;
import com.yegor256.xsline.Xsline;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@code dataized-to-const.xsl}.
 * @since 0.35.0
 */
final class DataizedToConstTest {

    @Test
    void doesNotConcatenatePayloadWithArgumentText() {
        MatcherAssert.assertThat(
            "a data literal's own payload text must not be glued to its argument's printed text",
            new Xsline(new StClasspath("/org/eolang/printer/print/dataized-to-const.xsl")).pass(
                new XMLDocument(
                    String.join(
                        "",
                        "<p><o base='.as-bytes'><o base='Φ.dataized'>",
                        "<o>01-02<o>5</o></o>",
                        "</o></o></p>"
                    )
                )
            ).xpath("//o[@const]/text()"),
            Matchers.contains("01-02")
        );
    }
}
