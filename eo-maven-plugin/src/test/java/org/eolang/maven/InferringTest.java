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
import java.util.List;
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
        Files.writeString(
            Files.createDirectories(this.dir.resolve("parsed")).resolve("main.xmir"),
            new EoSyntax(pack.map().get("eo").toString()).parsed().toString()
        );
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

    /**
     * The XPaths of the pack that match nothing, each named by the document it
     * was asked of.
     * @param pack The pack
     * @param temp The directory the clues have just written into
     * @return The XPaths that failed, empty when the pack is satisfied
     * @throws IOException If a document cannot be read
     */
    private Collection<String> unmatched(final Xtory pack, final Path temp) throws IOException {
        final Collection<String> failed = new ArrayList<>(0);
        for (final String key : Arrays.asList("xmir", "provides", "needs", "links")) {
            if (pack.map().containsKey(key)) {
                final XML written = this.written(temp, key);
                for (final String xpath : (List<String>) pack.map().get(key)) {
                    if (written.nodes(xpath).isEmpty()) {
                        failed.add(String.format("%s: %s", key, xpath));
                    }
                }
            }
        }
        return failed;
    }

    /**
     * The document the given key of a pack talks about.
     * @param temp The directory the clues have just written into
     * @param key The key, either {@code xmir} or the name of a table
     * @return The document
     * @throws IOException If it cannot be read
     */
    private XML written(final Path temp, final String key) throws IOException {
        final Path path;
        if ("xmir".equals(key)) {
            path = temp.resolve("pre").resolve("main.xmir");
        } else {
            path = temp.resolve("tables").resolve(String.format("%s.xml", key));
        }
        return new XMLDocument(path);
    }
}
