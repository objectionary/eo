/*
 * The MIT License (MIT)
 *
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
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.xembly.Directives;
import org.xembly.Xembler;

/**
 * Test case for {@link Xmir}.
 *
 * @since 0.5
 */
final class XmirTest {

    @Test
    void failsOnDispatchingAlphaAttributes() {
        Assertions.assertThrows(
            IllegalArgumentException.class,
            new Xmir(
                new XMLDocument(
                    new Xembler(
                        new Directives(new DrProgram("foo"))
                            .add("objects")
                            .add("o").attr("name", "foo")
                            .add("o").attr("base", ".α0").attr("name", "self")
                            .add("o").attr("base", "$")
                    ).xmlQuietly()
                )
            )::toEO,
            "XMIR with alpha dispatch should fail on converting to EO"
        );
    }

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
        final boolean conservative = xtory.map().containsKey("conservative");
        final Xmir xmir = this.asXmir((String) xtory.map().get("input"));
        MatcherAssert.assertThat(
            String.format(
                "Result PHI should be equal to provided PHI with syntax sugar, XMIR is:\n%s",
                xmir
            ),
            xmir.toPhi(conservative),
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
