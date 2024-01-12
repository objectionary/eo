/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2023 Objectionary.com
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
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.Map;
import org.cactoos.io.InputOf;
import org.cactoos.io.ResourceOf;
import org.cactoos.text.TextOf;
import org.eolang.jucs.ClasspathSource;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.yaml.snakeyaml.Yaml;

/**
 * Test case for {@link EoSyntax}.
 *
 * @since 0.1
 */
@SuppressWarnings("PMD.TooManyMethods")
final class EoSyntaxTest {
    @Test
    void parsesSimpleCode() throws Exception {
        MatcherAssert.assertThat(
            XhtmlMatchers.xhtml(
                new String(
                    new EoSyntax(
                        "test-1",
                        new ResourceOf("org/eolang/parser/fibonacci.eo")
                    ).parsed().toString().getBytes(),
                    StandardCharsets.UTF_8
                )
            ),
            XhtmlMatchers.hasXPaths(
                "/program[@name='test-1']",
                "/program[@ms and @time and @version]",
                "/program/listing",
                "/program/metas/meta[head='meta2']",
                "/program/objects/o[@name='fibo']"
            )
        );
    }

    @Test
    void printsProperListingEvenWhenSyntaxIsBroken() throws Exception {
        final String src = "# hello, world!\n\n[] > x-н, 1\n";
        MatcherAssert.assertThat(
            XhtmlMatchers.xhtml(
                new String(
                    new EoSyntax(
                        "test-44",
                        new InputOf(src)
                    ).parsed().toString().getBytes(StandardCharsets.UTF_8),
                    StandardCharsets.UTF_8
                )
            ),
            XhtmlMatchers.hasXPaths(
                "/program/errors[count(error)=2]",
                String.format("/program[listing='%s']", src)
            )
        );
    }

    @Test
    void copiesListingCorrectly() throws Exception {
        final String src = new TextOf(
            new ResourceOf("org/eolang/parser/factorial.eo")
        ).asString();
        final XML xml = new XMLDocument(
            new String(
                new EoSyntax(
                    "test-22",
                    new InputOf(src)
                ).parsed().toString().getBytes(),
                StandardCharsets.UTF_8
            )
        );
        MatcherAssert.assertThat(
            xml.xpath("/program/listing/text()"),
            Matchers.contains(src)
        );
    }

    @ParameterizedTest
    @ValueSource(strings = {
        "1 > x\n2 > y",
        "1 > x\r\n2 > y",
        "1 > x\r\n\r\n2 > y",
        "1 > x\n2 > y\n",
        "1 > x\n\n2 > y",
        "[]",
        "[] > x",
        "a b c > x\n  x ^ > @",
        "[] > x\n  x ^ > @"
    })
    void parsesSuccessfully(final String code) {
        final EoSyntax syntax = new EoSyntax(
            "test-2",
            new InputOf(code)
        );
        Assertions.assertDoesNotThrow(
            syntax::parsed
        );
    }

    @Test
    void parsesArrow() throws IOException {
        MatcherAssert.assertThat(
            new EoSyntax(
                "test-it-3",
                new InputOf("1 > x")
            ).parsed(),
            XhtmlMatchers.hasXPaths(
                "/program/objects/o[@base='int' and @name='x' and ends-with(text(), '1')]"
            )
        );
    }

    @Test
    void prasesNested() throws IOException {
        MatcherAssert.assertThat(
            new EoSyntax(
                "test-it-4",
                new InputOf(
                    "[] > base\n  memory 0 > x\n  [self] > f\n    v > @\n      v\n"
                )
            ).parsed(),
            XhtmlMatchers.hasXPaths(
                "/program/objects[count(o)=1]",
                "/program/objects/o[count(o)=2]"
            )
        );
    }

    @Test
    void parsesDefinition() throws IOException {
        MatcherAssert.assertThat(
            new EoSyntax(
                "test-it-5",
                new InputOf("[v] > p\n  f.write > @\n")
            ).parsed(),
            XhtmlMatchers.hasXPaths(
                "/program/objects[count(o)=1]",
                "/program/objects/o[count(o)=3]"
            )
        );
    }

    @Test
    void containsHash() throws IOException {
        MatcherAssert.assertThat(
            new EoSyntax(
                "test-it-5",
                new InputOf("[v] > p\n  f.write > @\n")
            ).parsed(),
            XhtmlMatchers.hasXPath("/program/@hash")
        );
    }

    @Test
    void parsesMethodCalls() throws IOException {
        MatcherAssert.assertThat(
            new EoSyntax(
                "test-it-1",
                new InputOf("add.\n  0\n  TRUE")
            ).parsed(),
            XhtmlMatchers.hasXPaths(
                "/program[@name='test-it-1']",
                "/program/objects/o[@base='.add']",
                "/program/objects/o/o[@base='int']",
                "/program/objects/o/o[@base='bool']"
            )
        );
    }

    @ParameterizedTest
    @ValueSource(strings = {
        "FALSE > false",
        "TRUE > true"
    })
    void storesAsBytes(final String code) throws IOException {
        final XML xml = new XMLDocument(
            new String(
                new EoSyntax(
                    "test-3",
                    new InputOf(code)
                ).parsed().toString().getBytes(),
                StandardCharsets.UTF_8
            )
        );
        MatcherAssert.assertThat(
            xml,
            XhtmlMatchers.hasXPaths(
                "/program/objects[count(o)=1]",
                "/program/objects/o[@data='bytes']"
            )
        );
    }

    @ParameterizedTest
    @ClasspathSource(value = "org/eolang/parser/typos/", glob = "**.yaml")
    void checksTypoPacks(final String yml) throws IOException, NumberFormatException {
        final Yaml yaml = new Yaml();
        final Map<String, Object> map = yaml.load(yml);
        Assumptions.assumeTrue(map.get("skip") == null);
        final XML xml = new EoSyntax(
            "typo",
            new InputOf(String.format("%s\n", map.get("eo")))
        ).parsed();
        MatcherAssert.assertThat(
            XhtmlMatchers.xhtml(xml.toString()),
            XhtmlMatchers.hasXPaths("/program/errors/error/@line")
        );
        MatcherAssert.assertThat(
            xml.toString(),
            Integer.parseInt(xml.xpath("/program/errors/error[1]/@line").get(0)),
            Matchers.equalTo(Integer.parseInt(map.get("line").toString()))
        );
    }
}
