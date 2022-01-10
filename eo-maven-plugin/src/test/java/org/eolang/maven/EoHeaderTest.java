/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2022 Yegor Bugayenko
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
package org.eolang.maven;

import org.cactoos.io.ResourceOf;
import org.hamcrest.MatcherAssert;
import org.junit.jupiter.api.Test;
import org.xmlunit.matchers.CompareMatcher;

/**
 * Test case for {@link EoHeader}.
 *
 * @since 0.21
 */
@SuppressWarnings("PMD.AvoidDuplicateLiterals")
final class EoHeaderTest {

    @Test
    void extracts() throws Exception {
        final EoHeader header = new EoHeader(
            new EoToXmir(
                new ResourceOf("org/eolang/maven/app.eo")
            ).toXml()
        );
        MatcherAssert.assertThat(
            header.toXml().toString(),
                CompareMatcher.isIdenticalTo(
                    String.join(
                        "\n",
                        "<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
                        "<header alias=\"sandbox.app\" name=\"app\">",
                        "   <free-attr name=\"n\"/>",
                        "   <free-attr name=\"args\" vararg=\"\"/>",
                        "</header>",
                        ""
                    )
                )
        );
    }
}
