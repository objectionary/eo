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
import com.jcabi.xml.XMLDocument;
import com.yegor256.xsline.Xsline;
import org.hamcrest.MatcherAssert;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link StUnhex}.
 *
 * @since 0.29.0
 */
final class StUnhexTest {

    @Test
    void convertsIntFromHexToEo() {
        MatcherAssert.assertThat(
            new Xsline(new StUnhex()).pass(
                new XMLDocument(
                    "<p><o base='int'><o base='org.eolang.bytes' data='bytes'>01 02 E4 F3 04 67 32 E1</o></o></p>"
                )
            ),
            XhtmlMatchers.hasXPaths("//o[text()='72872276393407201' and @data='int']")
        );
    }

    @Test
    void convertsMaxIntFromHexToEo() {
        MatcherAssert.assertThat(
            new Xsline(new StUnhex()).pass(
                new XMLDocument(
                    "<p><o base='int'><o base='org.eolang.bytes' data='bytes'>FF FF FF FF FF FF FF FF</o></o></p>"
                )
            ),
            XhtmlMatchers.hasXPaths("//o[text()='-1' and @data='int']")
        );
    }

    @Test
    void convertsStringFromHexToEo() {
        MatcherAssert.assertThat(
            new Xsline(new StUnhex()).pass(
                new XMLDocument(
                    String.join(
                        "",
                        "<p><o base='string'><o base='bytes' data='bytes'>41 42 0A 09</o></o>",
                        "<o base='string'><o base='bytes' data='bytes'>41 42</o></o></p>"
                    )
                )
            ),
            XhtmlMatchers.hasXPaths(
                "//o[text()='AB\\n\\t' and @data='string']",
                "//o[text()='AB' and @data='string']"
            )
        );
    }

    @Test
    void convertsEmptyStringFromHexToEo() {
        MatcherAssert.assertThat(
            new Xsline(new StUnhex()).pass(
                new XMLDocument(
                    "<p><o base='string'><o base='bytes' data='bytes'/></o></p>"
                )
            ),
            XhtmlMatchers.hasXPaths(
                "//o[empty(text()) and @data='string']"
            )
        );
    }

    @Test
    void convertsFloatFromHexToEo() {
        MatcherAssert.assertThat(
            new Xsline(new StUnhex()).pass(
                new XMLDocument(
                    "<p><o base='float'><o base='org.eolang.bytes' data='bytes'>41 42 43 67 AE CD 3E FD</o></o></p>"
                )
            ),
            XhtmlMatchers.hasXPaths(
                "//o[text()='2393807.3656386123' and @data='float']"
            )
        );
    }

    @Test
    void convertsTrueFromHexToEo() {
        MatcherAssert.assertThat(
            new Xsline(new StUnhex()).pass(
                new XMLDocument(
                    String.join(
                        "",
                        "<p><o base='bool' name='a'><o base='bytes' data='bytes'>01</o></o>",
                        "<o base='bool' name='b'><o base='bytes' data='bytes'>00</o></o></p>"
                    )
                )
            ),
            XhtmlMatchers.hasXPaths(
                "//o[text()='TRUE' and @data='bool' and @name='a']",
                "//o[text()='FALSE' and @data='bool' and @name='b']"
            )
        );
    }

    @Test
    void convertsFalseFromHexToEo() {
        MatcherAssert.assertThat(
            new Xsline(new StUnhex()).pass(
                new XMLDocument(
                    "<p><o base='bool'><o base='org.eolang.bytes' data='bytes'>00</o></o></p>"
                )
            ),
            XhtmlMatchers.hasXPaths("//o[text()='FALSE' and @data='bool']")
        );
    }

}
