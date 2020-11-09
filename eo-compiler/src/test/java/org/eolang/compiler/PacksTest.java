/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016 eolang.org
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
package org.eolang.compiler;

import com.jcabi.matchers.XhtmlMatchers;
import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import com.jcabi.xml.XSLChain;
import com.jcabi.xml.XSLDocument;
import java.io.ByteArrayOutputStream;
import java.util.Collection;
import java.util.Map;
import org.cactoos.io.InputOf;
import org.cactoos.io.OutputTo;
import org.cactoos.list.ListOf;
import org.cactoos.list.Mapped;
import org.hamcrest.MatcherAssert;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;
import org.yaml.snakeyaml.Yaml;

/**
 * Test case for packs.
 *
 * @since 1.0
 */
public final class PacksTest {

    private static Collection<String> yamlPacks() {
        return new ListOf<>(
            "simple.yaml",
            "catches-name-duplicates.yaml",
            "catches-alias-duplicates.yaml",
            "catches-unknown-names.yaml",
            "catches-self-naming.yaml",
            "catches-two-bodies.yaml",
            "catches-same-line-name.yaml",
            "catches-recursive-copy.yaml",
            "adds-refs.yaml",
            "resolves-aliases.yaml",
            "leap-year.yaml"
        );
    }

    @ParameterizedTest
    @MethodSource("yamlPacks")
    @SuppressWarnings("unchecked")
    public void testPacks(final String pack) throws Exception {
        Yaml yaml = new Yaml();
        final Map<String, Object> map = yaml.load(
            PacksTest.class.getResourceAsStream(
                String.format("packs/%s", pack)
            )
        );
        final ByteArrayOutputStream baos = new ByteArrayOutputStream();
        final Program program = new Program(
            new InputOf(String.format("%s\n", map.get("eo"))),
            new OutputTo(baos)
        );
        program.compile();
        final XML out = new XSLChain(
            new Mapped<>(
                node -> new XSLDocument(
                    ToJava.class.getResourceAsStream(node)
                ),
                (Collection<String>) map.get("xsls")
            )
        ).transform(new XMLDocument(baos.toString()));
        for (String xpath : (Collection<String>) map.get("tests")) {
            MatcherAssert.assertThat(
                XhtmlMatchers.xhtml(out.toString()),
                XhtmlMatchers.hasXPath(xpath)
            );
        }
    }

}
