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
import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import java.nio.file.Paths;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.DisabledOnOs;
import org.junit.jupiter.api.condition.OS;
import org.xembly.Directives;
import org.xembly.Xembler;

/**
 * Test case for {@link DrProgram}.
 *
 * @since 0.1
 */
final class DrProgramTest {

    @Test
    void buildsProgramElement() throws Exception {
        MatcherAssert.assertThat(
            "XMIR program element is built",
            XhtmlMatchers.xhtml(
                new Xembler(new DrProgram("foo")).xml()
            ),
            XhtmlMatchers.hasXPaths(
                "/program[@name='foo']",
                "/program[@dob and @time and @version and @revision]"
            )
        );
    }

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void setsSchemaLocation() throws Exception {
        final XML xml = new XMLDocument(new Xembler(new DrProgram("foo")).xml());
        MatcherAssert.assertThat(
            "XSD location is set",
            xml.toString(),
            Matchers.containsString(
                "xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\""
            )
        );
        final String url = xml.xpath("/program/@xsi:noNamespaceSchemaLocation").get(0);
        MatcherAssert.assertThat(
            "URL of XSD is set to file",
            url,
            Matchers.startsWith("file:///")
        );
        final String path = url.substring("file:///".length());
        MatcherAssert.assertThat(
            "XSD file exists",
            Paths.get(path).toFile().exists(),
            Matchers.is(true)
        );
    }

    @Test
    void validatesAgainstSchema() {
        MatcherAssert.assertThat(
            "XMIR document validates correctly",
            new XMLDocument(
                new Xembler(
                    new Directives().append(new DrProgram("foo"))
                        .add("listing").set("hello, world!").up()
                        .add("objects").add("o").attr("name", "bar")
                ).domQuietly()
            ).validate(),
            Matchers.emptyIterable()
        );
    }

}
