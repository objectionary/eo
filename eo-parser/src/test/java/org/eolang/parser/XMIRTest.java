/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2022 Objectionary.com
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

import com.jcabi.log.Logger;
import com.jcabi.xml.ClasspathSources;
import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import com.jcabi.xml.XSL;
import com.jcabi.xml.XSLDocument;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.Collection;
import org.cactoos.io.InputOf;
import org.cactoos.io.OutputTo;
import org.cactoos.io.ResourceOf;
import org.cactoos.iterable.Mapped;
import org.cactoos.list.ListOf;
import org.cactoos.text.TextOf;
import org.cactoos.text.UncheckedText;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

/**
 * Test case for {@link XMIR}.
 *
 * @since 0.5
 * @checkstyle AbbreviationAsWordInNameCheck (500 lines)
 */
public final class XMIRTest {

    @ParameterizedTest
    @MethodSource("samples")
    public void printsToEO(final String sample) throws Exception {
        final String src = new TextOf(
            new ResourceOf(sample)
        ).asString();
        Logger.debug(this, "Original EOLANG:%n%s", src);
        final XML first = XMIRTest.clean(XMIRTest.parse(src));
        Logger.debug(this, "First:%n%s", first);
        final String eolang = new XMIR(first).toEO();
        Logger.debug(this, "EOLANG:%n%s", eolang);
        final XML second = XMIRTest.clean(XMIRTest.parse(eolang));
        Logger.debug(this, "Second:%n%s", second);
        MatcherAssert.assertThat(
            first.toString(),
            Matchers.equalTo(second.toString())
        );
    }

    /**
     * Parse EO code to XMIR.
     * @param source The source
     * @return XMIR
     * @throws IOException If fails
     */
    private static XML parse(final String source) throws IOException {
        final ByteArrayOutputStream baos = new ByteArrayOutputStream();
        final Syntax syntax = new Syntax(
            "test", new InputOf(source), new OutputTo(baos)
        );
        syntax.parse();
        final XSL wrap = new XSLDocument(
            XMIRTest.class.getResourceAsStream("wrap-method-calls.xsl")
        ).with(new ClasspathSources());
        return wrap.transform(new XMLDocument(baos.toByteArray()));
    }

    /**
     * Take the clean version of XML, without the noise.
     * @param xmir The original
     * @return Clean one
     */
    private static XML clean(final XML xmir) {
        return new XSLDocument(
            XMIRTest.class.getResourceAsStream("strip-xmir.xsl")
        ).with(new ClasspathSources()).transform(xmir);
    }

    @SuppressWarnings("PMD.UnusedPrivateMethod")
    private static Collection<String> samples() {
        final String dir = "org/eolang/parser/xmir-samples/";
        return new ListOf<>(
            new Mapped<>(
                file -> String.format("%s%s", dir, file),
                new ListOf<>(
                    new UncheckedText(
                        new TextOf(
                            new ResourceOf(dir)
                        )
                    ).asString().split("\n")
                )
            )
        );
    }

}
