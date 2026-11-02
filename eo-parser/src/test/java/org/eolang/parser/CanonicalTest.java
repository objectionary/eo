/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import com.jcabi.matchers.XhtmlMatchers;
import com.jcabi.xml.ClasspathSources;
import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import com.jcabi.xml.XSL;
import com.jcabi.xml.XSLDocument;
import com.yegor256.xsline.Shift;
import com.yegor256.xsline.StClasspath;
import com.yegor256.xsline.StXSL;
import com.yegor256.xsline.TrClasspath;
import com.yegor256.xsline.TrDefault;
import com.yegor256.xsline.TrJoined;
import com.yegor256.xsline.Train;
import com.yegor256.xsline.Xsline;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import java.util.function.UnaryOperator;
import org.cactoos.io.InputOf;
import org.cactoos.io.ResourceOf;
import org.cactoos.text.TextOf;
import org.cactoos.text.UncheckedText;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Timeout;

/**
 * Test case for {@link Canonical}.
 *
 * @since 0.1
 */
final class CanonicalTest {

    @Test
    @Timeout(120L)
    void buildsALongTupleWithoutOverflowing() {
        final int size = 400;
        final StringBuilder xml = new StringBuilder(size * 24)
            .append("<object><o name='app'><o star='' line='1' pos='0'>");
        for (int idx = 0; idx < size; idx = idx + 1) {
            xml.append("<o base='Φ.e'/>");
        }
        MatcherAssert.assertThat(
            "a tuple literal must become one Φ.tuple layer per element, however many it holds, instead of overflowing Saxon as the layers are built",
            new Xsline(new StClasspath("/org/eolang/parser/parse/stars-to-tuples.xsl"))
                .pass(new XMLDocument(xml.append("</o></o></object>").toString()))
                .nodes("//o[@base='Φ.tuple']"),
            Matchers.hasSize(size)
        );
    }

    @Test
    void homesEveryBareAliasAttribute() {
        MatcherAssert.assertThat(
            "without the guards a bare alias is homed on base, atom, args, and type",
            CanonicalTest.apply(CanonicalTest.bare(), false),
            XhtmlMatchers.hasXPaths(
                "//o[@base='Φ.hello']",
                "//o[@atom='Φ.hello']",
                "//o[@args='Φ.hello']",
                "//o[@type='Φ.hello']"
            )
        );
    }

    @Test
    void keepsEveryBareAliasAttribute() {
        MatcherAssert.assertThat(
            "restored guards must keep that bare alias, so each removed predicate is really back",
            CanonicalTest.apply(CanonicalTest.bare(), true),
            XhtmlMatchers.hasXPaths(
                "//o[@base='hello']",
                "//o[@atom='hello']",
                "//o[@args='hello']",
                "//o[@type='hello']"
            )
        );
    }

    @Test
    void leavesSampledSourcesUnchanged() throws IOException {
        int missed = 0;
        for (final String source : CanonicalTest.samples()) {
            final String raw = new EoSyntax(
                new InputOf(source),
                UnaryOperator.identity()
            ).parsed().toString();
            if (!CanonicalTest.pass(raw, false).equals(CanonicalTest.pass(raw, true))) {
                missed = missed + 1;
            }
        }
        MatcherAssert.assertThat(
            "pack-shaped alias programs and runtime sources must keep the same canonical XMIR when the four alias guards are restored",
            missed,
            Matchers.equalTo(0)
        );
    }

    private static List<String> samples() {
        final String eol = System.lineSeparator();
        final Path repo = CanonicalTest.repo();
        return List.of(
            String.join(
                eol,
                "+alias hello org.example.hello",
                "+alias foo",
                "+alias bar org.eolang.number",
                "",
                "[] > app",
                "  hello > @",
                "  [] > fopen /file",
                "    ? > e /{foo}",
                "  [] > boxed /bar",
                "    ? >> h /bar?",
                ""
            ),
            CanonicalTest.text(repo.resolve("eo-runtime/src/main/eo/seq.eo")),
            CanonicalTest.text(repo.resolve("eo-runtime/src/main/eo/bytes.eo"))
        );
    }

    private static String bare() {
        return String.join(
            "",
            "<object><metas><meta><head>alias</head><part>hello</part>",
            "<part>Φ.org.example</part></meta></metas>",
            "<o base='hello'/><o atom='hello'/>",
            "<o args='hello'/><o type='hello'/></object>"
        );
    }

    private static XML apply(final String xml, final boolean guarded) {
        return CanonicalTest.sheet(guarded).transform(new XMLDocument(xml));
    }

    private static String pass(final String raw, final boolean guarded) {
        final int cut = Canonical.XSLS.indexOf(
            "/org/eolang/parser/parse/add-default-package.xsl"
        );
        final int end = Canonical.XSLS.size() - 1;
        return new Xsline(
            new TrFull(
                new TrJoined<>(
                    CanonicalTest.span(0, cut),
                    new TrDefault<Shift>(new StXSL(CanonicalTest.sheet(guarded))),
                    CanonicalTest.span(cut + 1, end),
                    new TrDefault<>(new StHex()),
                    CanonicalTest.span(end, end + 1)
                )
            )
        ).pass(new XMLDocument(raw)).toString();
    }

    private static Train<Shift> span(final int from, final int till) {
        return new TrClasspath<Shift>(
            Canonical.XSLS.subList(from, till).toArray(new String[0])
        ).back();
    }

    private static XSL sheet(final boolean guarded) {
        final String text = new UncheckedText(
            new TextOf(
                new ResourceOf(
                    "org/eolang/parser/parse/add-default-package.xsl",
                    CanonicalTest.class
                )
            )
        ).asString();
        final String body;
        if (guarded) {
            body = text.replace(
                ") then $t else",
                " or $t=/object/metas/meta[head='alias']/part[1]) then $t else"
            ).replace(
                ") then $type else",
                " or $type=/object/metas/meta[head='alias']/part[1]) then $type else"
            ).replace(
                "match=\"o\" mode=\"no-specials\"",
                "match=\"o[not(@base=/object/metas/meta[head='alias']/part[1])]\" mode=\"no-specials\""
            ).replace(
                "match=\"o\" mode=\"atom-no-specials\"",
                "match=\"o[not(@atom=/object/metas/meta[head='alias']/part[1])]\" mode=\"atom-no-specials\""
            );
        } else {
            body = text;
        }
        return new XSLDocument(body).with(new ClasspathSources()).with("objects", "");
    }

    private static String text(final Path path) {
        return new UncheckedText(new TextOf(path)).asString();
    }

    private static Path repo() {
        return Paths.get(System.getProperty("basedir")).toAbsolutePath().getParent();
    }
}
