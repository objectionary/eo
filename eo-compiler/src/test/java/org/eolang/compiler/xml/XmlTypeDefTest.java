/**
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
package org.eolang.compiler.xml;

import com.jcabi.matchers.XhtmlMatchers;
import java.util.Collections;
import org.cactoos.list.ListOf;
import org.eolang.compiler.syntax.Method;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.Test;
import org.xembly.Xembler;

/**
 * Test case for {@link XmlTypeDef}.
 *
 * @author Kirill (g4s8.public@gmail.com)
 * @version $Id$
 * @since 0.1
 * @checkstyle JavadocMethodCheck (500 lines)
 */
public final class XmlTypeDefTest {
    @Test
    public void compileToXml() throws Exception {
        MatcherAssert.assertThat(
            XhtmlMatchers.xhtml(
                new Xembler(
                    new XmlTypeDef(
                        "Book",
                        new ListOf<>(
                            "Text"
                        ),
                        new ListOf<>(
                            new Method(
                                "page",
                                Collections.emptyList(),
                                "Page"
                            )
                        )
                    )
                ).xml()
            ),
            Matchers.allOf(
                XhtmlMatchers.hasXPath("/type[@name = 'Book']"),
                XhtmlMatchers.hasXPath("/type/super/type[@name = 'Text']"),
                XhtmlMatchers.hasXPath("/type/methods[count(method) = 1]")
            )
        );
    }
}
