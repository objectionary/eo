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
            ).pass(new XMLDocument("<program><concurrency>no</concurrency></program>")).toString(),
            XhtmlMatchers.hasXPath("/program/sheets/sheet[text()='wrap-data']")
        );
    }

    @RepeatedTest(10)
    void addsSheetNameConcurrently() {
        final XML doc = new XMLDocument("<program><concurrency>yes</concurrency></program>");
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
            XhtmlMatchers.hasXPath("/program/sheets/sheet[text()='wrap-data']")
        );
    }
}
