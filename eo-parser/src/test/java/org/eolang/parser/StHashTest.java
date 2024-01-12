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
import com.yegor256.xsline.Xsline;
import java.security.NoSuchAlgorithmException;
import org.hamcrest.MatcherAssert;
import org.junit.jupiter.api.Test;
import org.xembly.Directives;
import org.xembly.Xembler;

/**
 * Test case for {@link StHash}.
 *
 * @since 0.35.0
 */
final class StHashTest {

    @Test
    void isHashInXml() {
        MatcherAssert.assertThat(
            "We should get XML, which has new attribute 'hash', but didn't",
            new Xsline(new StHash()).pass(program()),
            XhtmlMatchers.hasXPath("/program/@hash")
        );
    }

    @Test
    void checksHash() throws NoSuchAlgorithmException {
        MatcherAssert.assertThat(
            "We should get the same hash code, but didn't",
            new Xsline(new StHash()).pass(program()),
            XhtmlMatchers.hasXPath("/program/@hash", new StHash.Hash(program()).getHash())
        );
    }

    /**
     * Generates EO program for tests with specified time and context.
     * @return XML representation of program.
     */
    private static XML program() {
        return new XMLDocument(
            new Xembler(
                new Directives()
                    .add("program")
                    .attr("name", "main")
                    .add("objects")
                    .attr("object", "10")
                    .up()
            ).xmlQuietly()
        );
    }
}
