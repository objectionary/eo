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

import com.jcabi.xml.XMLDocument;
import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.nio.file.Path;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
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
    void validatesXmir(@Mktmp final Path tmp) throws Exception {
        MatcherAssert.assertThat(
            "validation should pass as normal",
            new StrictXmir(
                new Xmir(
                    new XMLDocument(
                        new Xembler(
                            new Directives()
                                .append(new DrProgram("foo"))
                                .xpath("/program")
                                .attr(
                                    "noNamespaceSchemaLocation xsi http://www.w3.org/2001/XMLSchema-instance",
                                    "https://www.eolang.org/XMIR.xsd"
                                )
                                .add("objects")
                        ).xml()
                    )
                ),
                tmp
            ).validate(),
            Matchers.emptyIterable()
        );
        MatcherAssert.assertThat(
            "temporary XSD file created",
            tmp.resolve("XMIR.xsd").toFile().exists(),
            Matchers.is(true)
        );
    }

    @Test
    @ExtendWith(MktmpResolver.class)
    void validatesXmirWithBrokenUri(@Mktmp final Path tmp) {
        Assertions.assertThrows(
            IllegalArgumentException.class,
            () -> new StrictXmir(
                new Xmir(
                    new XMLDocument(
                        new Xembler(
                            new Directives()
                                .append(new DrProgram("foobar"))
                                .xpath("/program")
                                .attr(
                                    "noNamespaceSchemaLocation xsi http://www.w3.org/2001/XMLSchema-instance",
                                    "https://www.invalid-website-uri/XMIR.xsd"
                                )
                                .add("objects")
                        ).xml()
                    )
                ),
                tmp
            ).validate(),
            "validation should fail because of broken URI"
        );
    }
}
