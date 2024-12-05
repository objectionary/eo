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
import com.jcabi.xml.StrictXML;
import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import com.yegor256.xsline.TrClasspath;
import com.yegor256.xsline.Xsline;
import org.cactoos.io.InputOf;
import org.eolang.jucs.ClasspathSource;
import org.eolang.xax.StoryMatcher;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.xembly.Xembler;

/**
 * Test case for {@link TrParsing}.
 *
 * @since 0.23
 */
final class TrParsingTest {

    @Test
    void buildsList() {
        MatcherAssert.assertThat(
            "ParsingTrain is not iterable with more than 1 item, but it must be",
            new TrParsing(),
            Matchers.iterableWithSize(Matchers.greaterThan(1))
        );
    }

    @Test
    void passesPipelineEvenWithErrors() {
        final XML xml = new XMLDocument(
            String.join(
                "\n",
                "<program>",
                "  <objects>",
                "    <o abstract=\"\" line=\"1\" name=\"main\" pos=\"0\">",
                "      <o base=\"bool\" data=\"bytes\" line=\"2\" name=\"x\" pos=\"2\">01</o>",
                "      <o base=\"bool\" data=\"bytes\" line=\"3\" name=\"x\" pos=\"2\">00</o>",
                "    </o>",
                "  </objects>",
                "</program>"
            )
        );
        MatcherAssert.assertThat(
            "XSL transformation works properly.",
            new Xsline(
                new TrParsing()
            ).pass(xml),
            XhtmlMatchers.hasXPaths(
                "/program/sheets[count(sheet)>1]",
                "/program[not(errors)]"
            )
        );
    }

    @ParameterizedTest
    @ClasspathSource(value = "org/eolang/parser/packs/", glob = "**.yaml")
    void parsesPacks(final String yaml) {
        MatcherAssert.assertThat(
            "passed without exceptions",
            yaml,
            new StoryMatcher(
                eo -> {
                    try {
                        return new StrictXML(
                            new EoSyntax(
                                "scenario",
                                new InputOf(String.format("%s\n", eo))
                            ).parsed()
                        );
                    } catch (final Exception ex) {
                        throw new IllegalArgumentException(ex);
                    }
                },
                new TrParsing().empty()
            )
        );
    }

    @ParameterizedTest
    @ClasspathSource(value = "org/eolang/parser/xax/", glob = "**.yml")
    void createsXaxStoryWithXslStylesheets(final String yaml) {
        MatcherAssert.assertThat(
            "passes with no exceptions",
            yaml,
            new StoryMatcher()
        );
    }

    @Test
    void setsBlankSchema() {
        MatcherAssert.assertThat(
            "Removes XSD schema from XMIR",
            new Xsline(
                new TrClasspath<>("/org/eolang/parser/blank-xsd-schema.xsl").back()
            ).pass(new XMLDocument(new Xembler(new DrProgram("foo")).xmlQuietly())),
            XhtmlMatchers.hasXPath(
                "/program[@xsi:noNamespaceSchemaLocation='https://www.eolang.org/xsd/XMIR-anything.xsd']"
            )
        );
    }
}
