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
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.function.UnaryOperator;
import java.util.stream.Stream;
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
    void keepsAliasResolutionInDedicatedStage() {
        MatcherAssert.assertThat(
            "resolver must own every alias-bearing attribute",
            CanonicalTest.stylesheet(
                "org/eolang/parser/parse/resolve-aliases.xsl"
            ),
            XhtmlMatchers.hasXPaths(
                "/*/*[local-name()='template' and contains(@match, '@base') and .//@*[contains(., \"head='alias'\")]]",
                "/*/*[local-name()='template' and contains(@match, '@atom') and .//@*[contains(., \"head='alias'\")]]",
                "/*/*[local-name()='template' and @match='@args' and .//@*[contains(., \"head='alias'\")]]",
                "/*/*[local-name()='template' and @match='@type' and .//@*[contains(., \"head='alias'\")]]"
            )
        );
    }

    @Test
    void keepsResolverBeforeDefaultPackage() {
        MatcherAssert.assertThat(
            "resolver must immediately precede default package homing",
            Canonical.XSLS.indexOf(
                "/org/eolang/parser/parse/resolve-aliases.xsl"
            ),
            Matchers.equalTo(
                Canonical.XSLS.indexOf(
                    "/org/eolang/parser/parse/add-default-package.xsl"
                ) - 1
            )
        );
    }

    @Test
    void keepsDefaultPackageFreeOfAliasRules() {
        MatcherAssert.assertThat(
            "default package homing must not resolve aliases",
            CanonicalTest.stylesheet(
                "org/eolang/parser/parse/add-default-package.xsl"
            ),
            Matchers.not(
                XhtmlMatchers.hasXPath(
                    "/*//@*[contains(., \"head='alias'\")]"
                )
            )
        );
    }

    @Test
    void homesABareAliasTheRemovedGuardsWouldHaveKept() {
        MatcherAssert.assertThat(
            "a bare alias name that still reaches add-default-package must be homed once the guards are gone",
            CanonicalTest.packaged(CanonicalTest.bareAlias(), false),
            XhtmlMatchers.hasXPaths(
                "//o[@base='Φ.hello']",
                "//o[@atom='Φ.hello']",
                "//o[@args='Φ.hello']",
                "//o[@type='Φ.hello']"
            )
        );
    }

    @Test
    void keepsABareAliasWhenTheGuardsAreRestored() {
        MatcherAssert.assertThat(
            "restoring the four alias guards must leave that same bare alias name untouched, so the comparison below is not vacuous",
            CanonicalTest.packaged(CanonicalTest.bareAlias(), true),
            XhtmlMatchers.hasXPaths(
                "//o[@base='hello']",
                "//o[@atom='hello']",
                "//o[@args='hello']",
                "//o[@type='hello']"
            )
        );
    }

    /**
     * Parses every eo-pack input and every eo-runtime source through two
     * copies of the canonical pipeline. The copies share every stylesheet
     * except {@code add-default-package.xsl}: one is the sheet in this
     * tree, the other is that sheet with the four removed alias guards put
     * back. Full canonical XMIR, including the sheets that follow, must
     * match.
     *
     * <p>Both copies leave the {@code objects} parameter empty. That covers
     * every other value of the parameter as well: a guard that fired would
     * keep a bare alias name, while the sheet without it would hand the
     * same name to {@code eo:homed} (or prefix {@code Φ.} on {@code @args}
     * and {@code @type}), and those two strings are never equal.</p>
     *
     * @throws IOException If a source cannot be read or parsed
     */
    @Test
    @Timeout(600L)
    void leavesPackAndRuntimeCanonicalOutputUnchanged() throws IOException {
        final List<Map.Entry<String, String>> programs = CanonicalTest.programs();
        final long runtime = programs.stream()
            .filter(program -> program.getKey().contains("eo-runtime"))
            .count();
        final long packs = programs.size() - runtime;
        MatcherAssert.assertThat(
            String.format(
                "the corpus must include the runtime sources and the packs, but had runtime=%d packs=%d",
                runtime,
                packs
            ),
            runtime > 160L && packs > 50L,
            Matchers.is(true)
        );
        final UnaryOperator<XML> live = CanonicalTest.pipeline(false);
        final UnaryOperator<XML> guarded = CanonicalTest.pipeline(true);
        for (final Map.Entry<String, String> program : programs) {
            final String raw = new EoSyntax(
                new InputOf(program.getValue()),
                UnaryOperator.identity()
            ).parsed().toString();
            MatcherAssert.assertThat(
                String.format(
                    "canonical XMIR of %s must not change when the alias guards are restored",
                    program.getKey()
                ),
                live.apply(new XMLDocument(raw)).toString(),
                Matchers.equalTo(guarded.apply(new XMLDocument(raw)).toString())
            );
        }
    }

    private static XML stylesheet(final String path) {
        return new XMLDocument(
            new UncheckedText(
                new TextOf(new ResourceOf(path, CanonicalTest.class))
            ).asString()
        );
    }

    private static String bareAlias() {
        return String.join(
            "",
            "<object><metas><meta><head>alias</head>",
            "<part>hello</part><part>Φ.org.example</part></meta></metas>",
            "<o base='hello'/><o atom='hello'/>",
            "<o args='hello'/><o type='hello'/></object>"
        );
    }

    private static XML packaged(final String xml, final boolean guarded) {
        return CanonicalTest.sheet(guarded).transform(new XMLDocument(xml));
    }

    private static UnaryOperator<XML> pipeline(final boolean guarded) {
        final int packaged = Canonical.XSLS.indexOf(
            "/org/eolang/parser/parse/add-default-package.xsl"
        );
        final int last = Canonical.XSLS.size() - 1;
        return new Xsline(
            new TrFull(
                new TrJoined<>(
                    CanonicalTest.train(0, packaged),
                    new TrDefault<Shift>(new StXSL(CanonicalTest.sheet(guarded))),
                    CanonicalTest.train(packaged + 1, last),
                    new TrDefault<>(new StHex()),
                    CanonicalTest.train(last, last + 1)
                )
            )
        )::pass;
    }

    private static Train<Shift> train(final int from, final int till) {
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
            body = CanonicalTest.restore(text);
        } else {
            body = text;
        }
        return new XSLDocument(body).with(new ClasspathSources()).with("objects", "");
    }

    private static String restore(final String text) {
        final String[] from = {
            "or $t=$eo:bottom) then $t else concat('Φ.', $t)",
            "or $type=$eo:bottom) then $type else concat('Φ.', $type)",
            "<xsl:template match=\"o\" mode=\"no-specials\">",
            "<xsl:template match=\"o\" mode=\"atom-no-specials\">",
        };
        final String[] to = {
            "or $t=$eo:bottom or $t=/object/metas/meta[head='alias']/part[1]) then $t else concat('Φ.', $t)",
            "or $type=$eo:bottom or $type=/object/metas/meta[head='alias']/part[1]) then $type else concat('Φ.', $type)",
            "<xsl:template match=\"o[not(@base=/object/metas/meta[head='alias']/part[1])]\" mode=\"no-specials\">",
            "<xsl:template match=\"o[not(@atom=/object/metas/meta[head='alias']/part[1])]\" mode=\"atom-no-specials\">",
        };
        String out = text;
        for (int idx = 0; idx < from.length; idx = idx + 1) {
            final String next = out.replace(from[idx], to[idx]);
            if (next.equals(out)) {
                throw new IllegalStateException(
                    String.format("alias guard #%d was not restored", idx)
                );
            }
            out = next;
        }
        return out;
    }

    private static List<Map.Entry<String, String>> programs() throws IOException {
        final Path module = CanonicalTest.module();
        final List<Map.Entry<String, String>> out = new ArrayList<>(0);
        CanonicalTest.collect(
            out,
            module.getParent().resolve(Paths.get("eo-runtime", "src", "main", "eo")),
            ".eo",
            false
        );
        CanonicalTest.collect(
            out,
            module.resolve(
                Paths.get(
                    "src", "test", "resources", "org", "eolang", "parser", "eo-packs"
                )
            ),
            ".yaml",
            true
        );
        return out;
    }

    private static void collect(
        final List<Map.Entry<String, String>> out,
        final Path root,
        final String suffix,
        final boolean pack
    ) throws IOException {
        try (Stream<Path> walk = Files.walk(root)) {
            final List<Path> files = walk
                .filter(path -> path.getFileName().toString().endsWith(suffix))
                .sorted()
                .toList();
            for (final Path path : files) {
                final String text = new UncheckedText(new TextOf(path)).asString();
                final String source;
                if (pack) {
                    source = CanonicalTest.packInput(text);
                } else {
                    source = text;
                }
                if (!source.isBlank()) {
                    out.add(Map.entry(path.toString(), source));
                }
            }
        }
    }

    private static String packInput(final String yaml) {
        final int at = yaml.indexOf("\ninput:");
        final String body;
        if (at < 0) {
            body = "";
        } else {
            final int nl = yaml.indexOf('\n', at + 1);
            if (nl < 0) {
                body = "";
            } else {
                body = CanonicalTest.unindent(yaml.substring(nl + 1));
            }
        }
        return body;
    }

    private static String unindent(final String body) {
        final StringBuilder out = new StringBuilder(body.length());
        for (final String line : body.split("\n", -1)) {
            final String plain = line.replace("\r", "");
            if (plain.startsWith("  ")) {
                out.append(plain.substring(2));
            } else {
                out.append(plain);
            }
            out.append('\n');
        }
        return out.toString();
    }

    private static Path module() {
        final Path cwd = Paths.get(System.getProperty("basedir", "")).toAbsolutePath();
        final Path packs = Paths.get(
            "src", "test", "resources", "org", "eolang", "parser", "eo-packs"
        );
        final Path found;
        if (Files.isDirectory(cwd.resolve(packs))) {
            found = cwd;
        } else if (Files.isDirectory(cwd.resolve("eo-parser").resolve(packs))) {
            found = cwd.resolve("eo-parser");
        } else {
            throw new IllegalStateException(
                String.format("eo-parser module was not found from %s", cwd)
            );
        }
        return found;
    }
}
