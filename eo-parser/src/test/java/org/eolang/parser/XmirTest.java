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
import com.jcabi.xml.XML;
import java.io.IOException;
import org.cactoos.io.InputOf;
import org.eolang.jucs.ClasspathSource;
import org.eolang.xax.XtSticky;
import org.eolang.xax.XtYaml;
import org.eolang.xax.Xtory;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.params.ParameterizedTest;

/**
 * Test case for {@link Xmir}.
 *
 * @since 0.5
 */
final class XmirTest {

    @ParameterizedTest
    @ClasspathSource(value = "org/eolang/parser/print-packs/yaml", glob = "**.yaml")
    void printsToEoStraight(final String pack) throws IOException {
        final Xtory xtory = new XtSticky(new XtYaml(pack));
        MatcherAssert.assertThat(
            "Result EO should be equal to original EO",
            this.asXmir((String) xtory.map().get("origin")).toEO(),
            Matchers.equalTo(xtory.map().get("straight"))
        );
    }

    @ParameterizedTest
    @ClasspathSource(value = "org/eolang/parser/print-packs/yaml", glob = "**.yaml")
    void printsToEoReversed(final String pack) throws IOException {
        final Xtory xtory = new XtSticky(new XtYaml(pack));
        MatcherAssert.assertThat(
            "Result EO should be equal to original EO in reverse notation",
            this.asXmir((String) xtory.map().get("origin")).toReversedEO(),
            Matchers.equalTo(xtory.map().get("reversed"))
        );
    }

    @ParameterizedTest
    @ClasspathSource(value = "org/eolang/parser/phi-packs", glob = "**.yaml")
    void convertsToSweetPhi(final String pack) throws IOException {
        final Xtory xtory = new XtSticky(new XtYaml(pack));
        final boolean conservative = xtory.map().containsKey("conservative");
        MatcherAssert.assertThat(
            "Result PHI should be equal to provided PHI with syntax sugar",
            this.asXmir((String) xtory.map().get("input")).toPhi(conservative),
            Matchers.equalTo(xtory.map().get("sweet"))
        );
    }

    @ParameterizedTest
    @ClasspathSource(value = "org/eolang/parser/phi-packs", glob = "**.yaml")
    void convertsToSaltyPhi(final String pack) throws IOException {
        final Xtory xtory = new XtSticky(new XtYaml(pack));
        MatcherAssert.assertThat(
            "Result PHI should be equal to provided PHI with syntax sugar",
            this.asXmir((String) xtory.map().get("input")).toSaltyPhi(),
            Matchers.equalTo(xtory.map().get("salty"))
        );
    }

    /**
     * Convert EO to XMIR.
     * @param program Program in EOLANG
     * @return XMIR
     */
    private Xmir asXmir(final String program) throws IOException {
        final XML xml = new EoSyntax("test", new InputOf(program)).parsed();
        MatcherAssert.assertThat(
            "Original EO should be parsed without errors",
            xml,
            Matchers.not(XhtmlMatchers.hasXPath("//errors/error"))
        );
        return new Xmir(xml);
    }
}
