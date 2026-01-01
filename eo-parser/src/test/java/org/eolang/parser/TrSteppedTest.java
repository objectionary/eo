/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import com.jcabi.matchers.XhtmlMatchers;
import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import com.jcabi.xml.XSL;
import com.jcabi.xml.XSLDocument;
import com.yegor256.Together;
import com.yegor256.xsline.Shift;
import com.yegor256.xsline.StClasspath;
import com.yegor256.xsline.TrDefault;
import com.yegor256.xsline.Xsline;
import org.cactoos.io.ResourceOf;
import org.cactoos.scalar.Sticky;
import org.cactoos.text.TextOf;
import org.hamcrest.MatcherAssert;
import org.junit.jupiter.api.RepeatedTest;
import org.junit.jupiter.api.Test;

/**
 * Test cases for {@link TrStepped}.
 *
 * @since 0.51
 */
final class TrSteppedTest {

    @Test
    void addsSheetName() {
        MatcherAssert.assertThat(
            "We expect the sheet name to be added",
            new Xsline(
                new TrStepped(
                    new TrDefault<Shift>().with(
                        new StClasspath("/org/eolang/parser/print/wrap-data.xsl")
                    )
                )
            ).pass(new XMLDocument("<object><concurrency>no</concurrency></object>")).toString(),
            XhtmlMatchers.hasXPath("/object/sheets/sheet[text()='wrap-data']")
        );
    }

    @RepeatedTest(10)
    void addsSheetNameConcurrently() {
        final XML doc = new XMLDocument("<object><concurrency>yes</concurrency></object>");
        final Sticky<XSL> loading = new Sticky<>(
            new TrStepped.Once<XSL>(
                () -> new XSLDocument(
                    new TextOf(
                        () -> new ResourceOf("org/eolang/parser/_stepped.xsl").stream()
                    ).asString()
                )
            )
        );
        MatcherAssert.assertThat(
            "We expect the sheet name to be added successfully in concurrent environment",
            new Together<>(
                i -> new Xsline(
                    new TrStepped(
                        new TrDefault<Shift>().with(
                            new StClasspath("/org/eolang/parser/print/wrap-data.xsl")
                        ),
                        loading
                    )
                ).pass(doc).toString()
            ).iterator().next(),
            XhtmlMatchers.hasXPath("/object/sheets/sheet[text()='wrap-data']")
        );
    }
}
