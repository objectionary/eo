/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.matchers.XhtmlMatchers;
import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import org.eolang.jucs.ClasspathSource;
import org.eolang.parser.EoSyntax;
import org.eolang.xax.XtSticky;
import org.eolang.xax.XtYaml;
import org.eolang.xax.Xtory;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;

/**
 * Test case for {@link Inferring}.
 *
 * <p>What the clues understand about a program is checked by the packs in
 * {@code inference-packs}: each one carries the EO source and the XPaths its
 * tables must satisfy, so a rule is described by the program it reads rather
 * than by XMIR written out by hand. What is left here are the mechanics no EO
 * source can express — how many files there are, where they land, and what
 * happens to a file whose source is gone.</p>
 *
 * @since 0.67.0
 */
@ExtendWith(MktmpResolver.class)
final class InferringTest {

    /**
     * Temp directory, injected into every test instance, since a
     * parameterized test cannot also take one as an argument.
     */
    @Mktmp
    private Path dir;

    @ParameterizedTest
    @ClasspathSource(value = "org/eolang/maven/inference-packs/", glob = "**.yaml")
    void understandsProgramOfPack(final String yaml) throws IOException {
        final Xtory pack = new XtSticky(new XtYaml(yaml));
        final Path parsed = Files.createDirectories(this.dir.resolve("parsed"));
        for (final Map.Entry<String, String> source : this.sources(pack).entrySet()) {
            Files.writeString(
                parsed.resolve(source.getKey()),
                new EoSyntax(source.getValue()).parsed().toString()
            );
        }
        new Inferring(
            this.dir.resolve("parsed"), this.dir.resolve("pre"), this.dir.resolve("tables")
        ).exec();
        MatcherAssert.assertThat(
            "every XPath of the pack must match what the clues wrote, but some didnt",
            this.unmatched(pack, this.dir),
            Matchers.empty()
        );
    }

    @Test
    void putsEveryFileOfProgramInOneTable(@Mktmp final Path temp) throws IOException {
        final Path many = Files.createDirectories(temp.resolve("many"));
        Files.writeString(
            many.resolve("kettle.xmir"),
            new EoSyntax(
                String.join(System.lineSeparator(), "[] > kettle", "  [] > steam", "")
            ).parsed().toString()
        );
        Files.createDirectories(many.resolve("nested"));
        Files.writeString(
            many.resolve("nested").resolve("cup.xmir"),
            new EoSyntax(
                String.join(System.lineSeparator(), "[] > cup", "  [] > handle", "")
            ).parsed().toString()
        );
        new Inferring(many, temp.resolve("all"), temp.resolve("tables")).exec();
        MatcherAssert.assertThat(
            "one table must cover the whole program, but a file was left out of it",
            new XMLDocument(temp.resolve("tables").resolve("provides.xml")),
            XhtmlMatchers.hasXPaths(
                "/provides/type[@id='Φ.kettle']/attr[@name='steam']",
                "/provides/type[@id='Φ.cup']/attr[@name='handle']"
            )
        );
    }

    @Test
    void forgetsSourceThatIsGone(@Mktmp final Path temp) throws IOException {
        final Path sources = Files.createDirectories(temp.resolve("shed"));
        Files.writeString(
            sources.resolve("rake.xmir"),
            new EoSyntax(
                String.join(System.lineSeparator(), "[] > rake", "  [] > teeth", "")
            ).parsed().toString()
        );
        new Inferring(sources, temp.resolve("pre"), temp.resolve("rows")).exec();
        Files.delete(sources.resolve("rake.xmir"));
        Files.writeString(
            sources.resolve("hoe.xmir"),
            new EoSyntax(
                String.join(System.lineSeparator(), "[] > hoe", "  [] > blade", "")
            ).parsed().toString()
        );
        new Inferring(sources, temp.resolve("pre"), temp.resolve("rows")).exec();
        MatcherAssert.assertThat(
            "a file whose source is gone must leave the table, but it stayed",
            new XMLDocument(temp.resolve("rows").resolve("provides.xml")),
            XhtmlMatchers.hasXPath("/provides[not(type[@id='Φ.rake'])]")
        );
    }

    @Test
    void keepsFoldersOfProgram(@Mktmp final Path temp) throws IOException {
        Files.writeString(
            Files.createDirectories(temp.resolve("tree").resolve("one")).resolve("leaf.xmir"),
            new EoSyntax(
                String.join(System.lineSeparator(), "[] > leaf", "  [] > vein", "")
            ).parsed().toString()
        );
        new Inferring(temp.resolve("tree"), temp.resolve("copy"), temp.resolve("rows")).exec();
        MatcherAssert.assertThat(
            "a file in a folder must be written to the same folder again, but it wasnt",
            Files.exists(temp.resolve("copy").resolve("one").resolve("leaf.xmir")),
            Matchers.is(true)
        );
    }

    @Test
    void ignoresADirectoryNamedLikeAnXmirFile(@Mktmp final Path temp) throws IOException {
        final Path sources = Files.createDirectories(temp.resolve("yard"));
        Files.writeString(
            sources.resolve("spade.xmir"),
            new EoSyntax(
                String.join(System.lineSeparator(), "[] > spade", "  [] > handle", "")
            ).parsed().toString()
        );
        Files.createDirectories(sources.resolve("stale.xmir"));
        new Inferring(sources, temp.resolve("pre"), temp.resolve("rows")).exec();
        MatcherAssert.assertThat(
            "a folder whose name ends with .xmir must be left alone, but it was read",
            new XMLDocument(temp.resolve("rows").resolve("provides.xml")),
            XhtmlMatchers.hasXPath("/provides/type[@id='Φ.spade']/attr[@name='handle']")
        );
    }

    private Map<String, String> sources(final Xtory pack) {
        final Map<String, String> found = new LinkedHashMap<>(0);
        for (final Map.Entry<?, ?> entry : ((Map<?, ?>) pack.map().get("eo")).entrySet()) {
            found.put(
                entry.getKey().toString().replace(".eo", ".xmir"),
                entry.getValue().toString()
            );
        }
        return found;
    }

    private Collection<String> unmatched(final Xtory pack, final Path temp) throws IOException {
        final Collection<String> failed = new ArrayList<>(0);
        final Collection<String> tables = Arrays.asList(
            "provides", "needs", "links"
        );
        for (final Object key : pack.map().keySet()) {
            if (!"eo".equals(key) && !"xmir".equals(key) && !tables.contains(key)) {
                failed.add(String.format("unknown key: %s", key));
            }
        }
        for (final String table : tables) {
            if (pack.map().containsKey(table)) {
                failed.addAll(
                    this.absent(
                        new XMLDocument(temp.resolve("tables").resolve(table.concat(".xml"))),
                        table,
                        (List<String>) pack.map().get(table)
                    )
                );
            }
        }
        failed.addAll(this.unprepared(pack, temp));
        return failed;
    }

    private Collection<String> unprepared(final Xtory pack, final Path temp) throws IOException {
        final Collection<String> failed = new ArrayList<>(0);
        if (pack.map().containsKey("xmir")) {
            final Map<String, String> sources = this.sources(pack);
            for (final Map.Entry<?, ?> entry : ((Map<?, ?>) pack.map().get("xmir")).entrySet()) {
                final String name = entry.getKey().toString().replace(".eo", ".xmir");
                if (sources.containsKey(name)) {
                    failed.addAll(
                        this.absent(
                            new XMLDocument(temp.resolve("pre").resolve(name)),
                            entry.getKey().toString(),
                            (List<String>) entry.getValue()
                        )
                    );
                } else {
                    failed.add(String.format("unknown file: %s", entry.getKey()));
                }
            }
        }
        return failed;
    }

    private Collection<String> absent(
        final XML document, final String about, final List<String> xpaths
    ) {
        final Collection<String> failed = new ArrayList<>(0);
        for (final String xpath : xpaths) {
            if (document.nodes(xpath).isEmpty()) {
                failed.add(String.format("%s: %s", about, xpath));
            }
        }
        return failed;
    }
}
