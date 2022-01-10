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

import com.jcabi.xml.XML;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import org.cactoos.io.InputOf;
import org.cactoos.io.OutputTo;
import org.cactoos.io.ResourceOf;
import org.hamcrest.MatcherAssert;
import org.hamcrest.core.IsEqual;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.xmlunit.matchers.CompareMatcher;

/**
 * Test case for {@link EoHeaderRegistry}.
 *
 * @since 0.21
 * @checkstyle ClassDataAbstractionCouplingCheck (500 lines)
 */
@SuppressWarnings("PMD.AvoidDuplicateLiterals")
final class EoHeaderRegistryTest {

    /**
     * A sample of xmir.
     */
    private static XML xmir;

    @BeforeAll
    static void setUp() throws IOException {
        EoHeaderRegistryTest.xmir = new EoToXmir(
            new ResourceOf("org/eolang/maven/app.eo")
        ).toXml();
    }

    @Test
    void registersHeaders() {
        final ByteArrayOutputStream stdout = new ByteArrayOutputStream();
        new EoHeaderRegistry(
            new EoHeader(EoHeaderRegistryTest.xmir),
            new InputOf(
                String.join(
                    "\n",
                    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
                    "<headers>",
                    "   <header alias=\"org.eolang.io.stdout\" name=\"stdout\">",
                    "      <free-attr name=\"text\"/>",
                    "   </header>",
                    "</headers>",
                    ""
                )
            ),
            new OutputTo(stdout)
        ).register();
        MatcherAssert.assertThat(
            new String(stdout.toByteArray(), StandardCharsets.UTF_8),
            CompareMatcher.isIdenticalTo(
                String.join(
                    "\n",
                    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
                    "<headers>",
                    "   <header alias=\"org.eolang.io.stdout\" name=\"stdout\">",
                    "      <free-attr name=\"text\"/>",
                    "   </header>",
                    "   <header alias=\"sandbox.app\" name=\"app\">",
                    "      <free-attr name=\"n\"/>",
                    "      <free-attr name=\"args\" vararg=\"\"/>",
                    "   </header>",
                    "</headers>",
                    ""
                )
            ).ignoreWhitespace()
        );
    }

    @Test
    void avoidsHeaderDuplication() {
        final String registry = String.join(
            "\n",
            "<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
            "<headers>",
            "   <header alias=\"org.eolang.io.stdout\" name=\"stdout\">",
            "      <free-attr name=\"text\"/>",
            "   </header>",
            "   <header alias=\"sandbox.app\" name=\"app\">",
            "      <free-attr name=\"n\"/>",
            "      <free-attr name=\"args\" vararg=\"\"/>",
            "   </header>",
            "</headers>",
            ""
        );
        final ByteArrayOutputStream stdout = new ByteArrayOutputStream();
        new EoHeaderRegistry(
            new EoHeader(EoHeaderRegistryTest.xmir),
            new InputOf(registry),
            new OutputTo(stdout)
        ).register();
        MatcherAssert.assertThat(
            new String(stdout.toByteArray(), StandardCharsets.UTF_8),
            new IsEqual<>(registry)
        );
    }
}
