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

import com.jcabi.manifests.Manifests;
import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import com.yegor256.Together;
import com.yegor256.WeAreOnline;
import java.nio.file.Path;
import java.nio.file.Paths;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.RepeatedTest;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.xembly.Directives;
import org.xembly.Xembler;

/**
 * Test case for {@link StrictXmir}.
 *
 * @since 0.5
 */
final class StrictXmirTest {

    @Test
    @ExtendWith(MktmpResolver.class)
    @ExtendWith(WeAreOnline.class)
    void validatesXmir(@Mktmp final Path tmp) {
        Assertions.assertDoesNotThrow(
            new StrictXmir(
                StrictXmirTest.xmir("https://www.eolang.org/XMIR.xsd"),
                tmp
            )::inner,
            "validation should pass as normal"
        );
        MatcherAssert.assertThat(
            "temporary XSD file created",
            tmp.resolve("XMIR.xsd").toFile().exists(),
            Matchers.is(true)
        );
    }

    @RepeatedTest(20)
    @ExtendWith(WeAreOnline.class)
    @ExtendWith(MktmpResolver.class)
    void doesNotFailWithDifferentXmlInMultipleThreads(@Mktmp final Path tmp) {
        Assertions.assertDoesNotThrow(
            new Together<>(
                thread -> {
                    final XML xml = StrictXmirTest.xmir("https://www.eolang.org/XMIR.xsd");
                    return new StrictXmir(xml, tmp).inner();
                }
            )::asList,
            "StrictXmir should not fail in different threads with different xmls"
        );
    }

    @RepeatedTest(20)
    @ExtendWith(WeAreOnline.class)
    @ExtendWith(MktmpResolver.class)
    void doesNotFailOnTheSameOperation(@Mktmp final Path tmp) {
        final XML xmir = new StrictXmir(
            StrictXmirTest.xmir("https://www.eolang.org/XMIR.xsd"), tmp
        );
        Assertions.assertDoesNotThrow(
            new Together<>(
                thread -> xmir.inner()
            )::asList,
            "StrictXmir should not fail in different threads with the same xml"
        );
    }

    @Test
    @ExtendWith(MktmpResolver.class)
    @ExtendWith(WeAreOnline.class)
    void refersToAbsoluteFileName(@Mktmp final Path tmp) {
        MatcherAssert.assertThat(
            "XSD location must be absolute",
            Paths.get(
                new StrictXmir(StrictXmirTest.xmir("https://www.eolang.org/XMIR.xsd"), tmp)
                    .xpath("/program/@xsi:noNamespaceSchemaLocation")
                    .get(0)
                    .substring("file:///".length())
            ).isAbsolute(),
            Matchers.is(true)
        );
    }

    @Test
    @ExtendWith(MktmpResolver.class)
    void validatesXmirWithLocalSchema(@Mktmp final Path tmp) {
        Assertions.assertDoesNotThrow(
            new StrictXmir(
                new Xmir(
                    StrictXmirTest.xmir(
                        String.format(
                            "https://www.eolang.org/xsd/XMIR-%s.xsd",
                            Manifests.read("EO-Version")
                        )
                    )
                ),
                tmp
            )::inner,
            "validation should pass as normal"
        );
        MatcherAssert.assertThat(
            "temporary XSD file created",
            tmp.resolve(
                String.format("XMIR-%s.xsd", Manifests.read("EO-Version"))
            ).toFile().exists(),
            Matchers.is(true)
        );
    }

    @Test
    @ExtendWith(MktmpResolver.class)
    void validatesXmirWithBrokenUri(@Mktmp final Path tmp) {
        Assertions.assertThrows(
            IllegalArgumentException.class,
            new StrictXmir(
                new Xmir(
                    StrictXmirTest.xmir("https://www.invalid-website-uri/XMIR.xsd")
                ),
                tmp
            )::inner,
            "validation should fail because of broken URI"
        );
    }

    /**
     * Make a simple XMIR.
     * @param schema The schema
     */
    private static XML xmir(final String schema) {
        return new XMLDocument(
            new Xembler(
                new Directives()
                    .append(new DrProgram("foo"))
                    .xpath("/program")
                    .attr(
                        "noNamespaceSchemaLocation xsi http://www.w3.org/2001/XMLSchema-instance",
                        schema
                    )
                    .add("objects")
            ).xmlQuietly()
        );
    }
}
