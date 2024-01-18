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
package org.eolang.parser.xmir;

import com.jcabi.log.Logger;
import com.jcabi.matchers.XhtmlMatchers;
import com.jcabi.xml.XML;
import java.io.IOException;
import java.util.Map;
import java.util.function.Function;
import org.cactoos.io.InputOf;
import org.eolang.jucs.ClasspathSource;
import org.eolang.parser.EoSyntax;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.params.ParameterizedTest;
import org.yaml.snakeyaml.Yaml;

/**
 * Test case for {@link Xmir} and {@link XmirReversed}.
 *
 * @since 0.5
 * @checkstyle AbbreviationAsWordInNameCheck (500 lines)
 */
final class XmirTest {
    @ParameterizedTest
    @ClasspathSource(value = "org/eolang/parser/samples/", glob = "**.yaml")
    void printsStrait(final String pack) throws IOException {
        final Map<String, Object> map = new Yaml().load(pack);
        final String key = "strait";
        final String eolang = this.eolang((String) map.get("origin"), Xmir.Default::new);
        MatcherAssert.assertThat(
            "Result EO should be parsed without errors",
            new EoSyntax("test", new InputOf(eolang)).parsed(),
            Matchers.not(XhtmlMatchers.hasXPath("//errors/error"))
        );
        MatcherAssert.assertThat(
            String.format(
                "Result EO should be equal to original EO in %s notation",
                key
            ),
            map.get(key),
            Matchers.equalTo(eolang)
        );
    }

    @ParameterizedTest
    @ClasspathSource(value = "org/eolang/parser/samples/", glob = "**.yaml")
    void printsReversed(final String pack) throws IOException {
        final Map<String, Object> map = new Yaml().load(pack);
        final String key = "reversed";
        final String eolang = this.eolang((String) map.get("origin"), XmirReversed::new);
        MatcherAssert.assertThat(
            "Result EO should be parsed without errors",
            new EoSyntax("test", new InputOf(eolang)).parsed(),
            Matchers.not(XhtmlMatchers.hasXPath("//errors/error"))
        );
        MatcherAssert.assertThat(
            String.format(
                "Result EO should be equal to original EO in %s notation",
                key
            ),
            map.get(key),
            Matchers.equalTo(eolang)
        );
    }

    /**
     * Parse EO from given pack, converts XMIR to EO.
     * @param original Original EOLANG
     * @param xmir Xmir constructor
     * @return Eolang from XMIR
     */
    private String eolang(
        final String original,
        final Function<XML, Xmir> xmir
    ) throws IOException {
        Logger.debug(this, "Original EOLANG:%n%s", original);
        final XML first = new EoSyntax("test", new InputOf(original)).parsed();
        MatcherAssert.assertThat(
            "Original EO should be parsed without errors",
            first,
            Matchers.not(XhtmlMatchers.hasXPath("//errors/error"))
        );
        Logger.debug(this, "First:%n%s", first);
        final String eolang = xmir.apply(first).toEO();
        Logger.debug(this, "EOLANG:%n%s", eolang);
        return eolang;
    }
}
