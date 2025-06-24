/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import com.jcabi.matchers.XhtmlMatchers;
import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import java.io.IOException;
import org.cactoos.io.InputOf;
import org.eolang.jucs.ClasspathSource;
import org.eolang.xax.XtSticky;
import org.eolang.xax.XtYaml;
import org.eolang.xax.Xtory;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;

/**
 * Test case for {@link Xmir}.
 *
 * @since 0.5
 */
final class XmirTest {

    @ParameterizedTest
    @ClasspathSource(value = "org/eolang/parser/print-packs/yaml", glob = "**.yaml")
    void printsToEo(final String pack) throws IOException {
        final Xtory xtory = new XtSticky(new XtYaml(pack));
        final Xmir xmir = this.asXmir((String) xtory.map().get("origin"));
        MatcherAssert.assertThat(
            String.format(
                "Result EO should be equal to original EO, XMIR is:\n%s",
                xmir
            ),
            xmir.toEO(),
            Matchers.equalTo(xtory.map().get("printed"))
        );
    }

    @ParameterizedTest
    @ClasspathSource(value = "org/eolang/parser/phi-packs", glob = "**.yaml")
    void convertsToSweetPhi(final String pack) throws IOException {
        final Xtory xtory = new XtSticky(new XtYaml(pack));
        final Xmir xmir = this.asXmir((String) xtory.map().get("input"));
        MatcherAssert.assertThat(
            String.format(
                "Result PHI should be equal to provided PHI with syntax sugar, XMIR is:\n%s",
                xmir
            ),
            xmir.toPhi(),
            Matchers.equalTo(xtory.map().get("sweet"))
        );
    }

    @ParameterizedTest
    @ClasspathSource(value = "org/eolang/parser/phi-packs", glob = "**.yaml")
    void convertsToSaltyPhi(final String pack) throws IOException {
        final Xtory xtory = new XtSticky(new XtYaml(pack));
        final Xmir xmir = this.asXmir((String) xtory.map().get("input"));
        MatcherAssert.assertThat(
            String.format(
                "Result PHI should be equal to provided PHI without syntax sugar, XMIR is:\n%s",
                xmir
            ),
            xmir.toSaltyPhi(),
            Matchers.equalTo(xtory.map().get("salty"))
        );
    }

    @Test
    void preservesNumbersInPhiRepresentation() throws IOException {
        final String phi = this.asXmir("1 > foo\n").toPhi();
        MatcherAssert.assertThat(
            String.format("Phi expression should contain the data, but it doesn't: %n%s", phi),
            phi,
            Matchers.containsString("foo ↦ 1")
        );
    }

    @Test
    void skips() throws IOException {
        final String xml = String.join(
            "\n",
            "<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
            "<object>",
            "  <metas>",
            "    <meta>",
            "      <head>alias</head>",
            "      <tail>j$foo</tail>",
            "      <part>j$foo</part>",
            "    </meta>",
            "  </metas>",
            "  <o name='j$foo'>",
            "    <o name='j$AbstractParent' base='Q.jeo.class'>",
            "      <o name='j$foo' base='Q.jeo.method'>",
            "        <o name='signature'>\"\"</o>",
            "      </o>",
            "    </o>",
            "  </o>",
            "</object>"
        );
        final Xmir xmir = new Xmir(new XMLDocument(xml));
        final String phi = xmir.toPhi();
        final String salty = xmir.toSaltyPhi();
        MatcherAssert.assertThat(
            "Sweet PHI should not contain 'Φ.j$foo'",
            phi,
            Matchers.not(Matchers.containsString("Φ.j$foo"))
        );
        MatcherAssert.assertThat(
            "Salty PHI should not contain 'Φ.j$foo'",
            salty,
            Matchers.not(Matchers.containsString("Φ.j$foo"))
        );
        MatcherAssert.assertThat(
            "PHI should contain 'j$foo ↦ ⟦'",
            phi,
            Matchers.containsString("j$foo ↦ ⟦")
        );
        MatcherAssert.assertThat(
            "PHI should contain 'j$foo ↦ Φ.jeo.method'",
            phi,
            Matchers.containsString("j$foo ↦ Φ.jeo.method")
        );
    }

    /**
     * Convert EO to XMIR.
     * @param program Program in EOLANG
     * @return XMIR
     */
    private Xmir asXmir(final String program) throws IOException {
        final XML xml = new EoSyntax(new InputOf(program)).parsed();
        MatcherAssert.assertThat(
            "Original EO should be parsed without errors",
            xml,
            Matchers.not(XhtmlMatchers.hasXPath("//errors/error"))
        );
        return new Xmir(xml);
    }
}
