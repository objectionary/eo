/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import com.jcabi.matchers.XhtmlMatchers;
import com.jcabi.xml.XML;
import com.yegor256.xsline.TrDefault;
import java.io.IOException;
import org.eolang.jucs.ClasspathSource;
import org.eolang.xax.XtSticky;
import org.eolang.xax.XtStrictAfter;
import org.eolang.xax.XtYaml;
import org.eolang.xax.Xtory;
import org.eolang.xax.XtoryMatcher;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.MethodSource;
import org.xembly.Directives;

/**
 * Test cases for {@link PhiSyntax}.
 *
 * @since 0.35.0
 */
final class PhiSyntaxTest {
    @ParameterizedTest
    @CsvSource({
        "empty ↦ Φ.org.eolang.bytes, program structure is invalid",
        "{⟦obj ↦ ⟦⟧(x ↦ ⟦⟧)⟧}, application on formation is not supported",
        "{⟦obj ↦ ⟦a ↦ ⟦⟧.y(Δ ⤍ 00-)⟧⟧}, delta application is not supported"
    })
    void addsError(final String program, final String message) throws IOException {
        MatcherAssert.assertThat(
            String.format("Result XML must contain errors because %s", message),
            new PhiSyntax(program).parsed(),
            XhtmlMatchers.hasXPath("//errors[count(error)>0]")
        );
    }

    @Test
    void addsExtra() throws IOException {
        MatcherAssert.assertThat(
            "Result XML must contain extra object",
            new PhiSyntax(
                () -> "{⟦obj ↦ ⟦⟧⟧}",
                new Directives().xpath("/object/o").add("o").attr("base", "x")
            ).parsed(),
            XhtmlMatchers.hasXPath(
                "/object/o/o[@base='x']"
            )
        );
    }

    @Test
    void addsMetaForPackage() throws IOException {
        MatcherAssert.assertThat(
            "XMIR contains meta with package",
            new PhiSyntax(
                "{⟦foo ↦ ⟦bar ↦ Φ.org.eolang.bytes(α0 ↦ ⟦ Δ ⤍ 42- ⟧), λ ⤍ Package⟧⟧}"
            ).parsed(),
            XhtmlMatchers.hasXPath(
                "/object/metas/meta[@line='1' and head='package' and tail='foo']",
                "/object/objects/o[@base='Q']",
                "/object/o[@base='.org' and @method]",
                "/object/o[@base='.eolang' and @method]"
            )
        );
    }

    @ParameterizedTest
    @ClasspathSource(value = "org/eolang/parser/phi-packs", glob = "**.yaml")
    void printsSaltyToSweet(final String pack) throws IOException {
        final Xtory xtory = new XtSticky(new XtYaml(pack));
        MatcherAssert.assertThat(
            "Salty XMIR should be equivalent to sweet one",
            new Xmir(
                new PhiSyntax((String) xtory.map().get("salty")).parsed()
            ).toPhi(xtory.map().get("conservative") != null),
            Matchers.equalTo(xtory.map().get("sweet"))
        );
    }

    @ParameterizedTest
    @ClasspathSource(value = "org/eolang/parser/phi-packs", glob = "**.yaml")
    void printsSweetToSalty(final String pack) throws IOException {
        final Xtory xtory = new XtSticky(new XtYaml(pack));
        MatcherAssert.assertThat(
            "Sweet XMIR should be equivalent to salty one",
            new Xmir(
                new PhiSyntax((String) xtory.map().get("sweet")).parsed()
            ).toSaltyPhi(),
            Matchers.equalTo(xtory.map().get("salty"))
        );
    }

    @ParameterizedTest
    @ClasspathSource(value = "org/eolang/parser/unphi-packs/", glob = "**.yaml")
    void checksUnphiPacks(final String yaml) {
        MatcherAssert.assertThat(
            "passed without exceptions",
            new XtSticky(
                new XtStrictAfter(
                    new XtYaml(
                        yaml,
                        phi -> new PhiSyntax(
                            String.format("%s\n", phi), new TrDefault<>()
                        ).parsed(),
                        new TrFull()
                    )
                )
            ),
            new XtoryMatcher()
        );
    }

    @ParameterizedTest
    @ClasspathSource(value = "org/eolang/parser/phi-syntax/", glob = "**.phi")
    void checksValidPhiExpressions(final String phi) throws IOException {
        final XML xml = new StrictXmir(new PhiSyntax(phi).parsed());
        MatcherAssert.assertThat(
            "syntax is valid, can be parsed without errors",
            XhtmlMatchers.xhtml(xml.toString()),
            XhtmlMatchers.hasXPaths("/object[not(errors)]")
        );
    }

    @ParameterizedTest
    @ClasspathSource(value = "org/eolang/parser/phi-typos/", glob = "**.phi")
    void checksBrokenPhiExpressions(final String phi) throws IOException {
        final XML xml = new StrictXmir(new PhiSyntax(phi).parsed());
        MatcherAssert.assertThat(
            "syntax is broken, can't be parsed without errors",
            XhtmlMatchers.xhtml(xml.toString()),
            XhtmlMatchers.hasXPaths("/object/errors/error/@line")
        );
    }

    @ParameterizedTest
    @MethodSource("org.eolang.parser.EoSyntaxTest#naughty")
    void parsesNaughtyString(final String input) throws IOException {
        MatcherAssert.assertThat(
            String.format("Failed to understand string: %s", input),
            new PhiSyntax(String.format("{[[ string -> \"%s\" ]]}", input)).parsed(),
            XhtmlMatchers.hasXPath("/object[not(errors)]")
        );
    }
}
