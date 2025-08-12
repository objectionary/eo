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
import org.xembly.Directives;
import org.xembly.ImpossibleModificationException;
import org.xembly.Xembler;

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

    @Test
    void restoresApostropheForIdempotency() throws ImpossibleModificationException {
        MatcherAssert.assertThat(
            "Apostrophe is not restored, but should be",
            new Xmir(
                new XMLDocument(
                    new Xembler(
                        new Directives()
                            .add("object")
                            .add("o")
                            .attr("base", "Φ.org.eolang.start")
                            .attr("name", "φ")
                            .add("o")
                            .attr("base", "Φ.org.eolang.random")
                            .attr("name", "r")
                            .add("o")
                            .attr("base", "ξ.xi🌵")
                    ).xml()
                )
            ).toEO(),
            Matchers.containsString(
                "random > r'"
            )
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
