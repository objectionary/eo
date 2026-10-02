/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import com.yegor256.xsline.StClasspath;
import com.yegor256.xsline.TrDefault;
import com.yegor256.xsline.Xsline;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
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
 * Test case for {@link StPure} and the {@code purify.xsl} it runs.
 *
 * <p>Which formations are marked is checked by the packs in
 * {@code purify-packs}: each one carries the EO sources of a whole program
 * and the XPaths the stamped XMIR must satisfy, with a formation that must
 * not be marked written as {@code not(@pure)}. The chain starts from EO
 * source and runs all of it — parsing, inference, stamping — so every rule of
 * the stylesheet is described by a program a reader can run in their head,
 * and a regression in any of the three stages shows up as a failed pack.</p>
 *
 * @since 0.75.0
 */
@ExtendWith(MktmpResolver.class)
final class StPureTest {

    /**
     * Temp directory, injected into every test instance, since a
     * parameterized test cannot also take one as an argument.
     */
    @Mktmp
    private Path dir;

    @ParameterizedTest
    @ClasspathSource(value = "org/eolang/maven/purify-packs/", glob = "**.yaml")
    void labelsFormationsOfPack(final String yaml) throws IOException {
        MatcherAssert.assertThat(
            "every XPath of the pack must match the stamped XMIR, but some didnt",
            this.unmatched(new XtSticky(new XtYaml(yaml))),
            Matchers.empty()
        );
    }

    @Test
    void changesNothingWithoutTables(@Mktmp final Path temp) throws IOException {
        final Path parsed = Files.createDirectories(temp.resolve("parsed"));
        final Path source = parsed.resolve("app.xmir");
        Files.writeString(
            source,
            new EoSyntax(
                String.join(System.lineSeparator(), "[x] > app", "  x > @", "")
            ).parsed().toString()
        );
        MatcherAssert.assertThat(
            "a build that skips inference has no tables to read, so nothing must be marked",
            StPureTest.stamped(temp.resolve("absent"), source).nodes("//o[@pure]"),
            Matchers.empty()
        );
    }

    @Test
    void marksWhatTheTablesSay(@Mktmp final Path temp) throws IOException {
        final Path parsed = StPureTest.program(temp);
        final Path tables = temp.resolve("tables");
        new Inferring(parsed, temp.resolve("pre"), tables).exec();
        MatcherAssert.assertThat(
            "the tables of this program mark its application, but nothing was marked",
            StPureTest.stamped(tables, parsed.resolve("app.xmir"))
                .nodes("//o[@name='x' and @pure='true']"),
            Matchers.not(Matchers.empty())
        );
    }

    @Test
    void readsATableThatWasRewritten(@Mktmp final Path temp) throws IOException {
        final Path parsed = StPureTest.program(temp);
        final Path tables = temp.resolve("tables");
        final Path source = parsed.resolve("app.xmir");
        new Inferring(parsed, temp.resolve("pre"), tables).exec();
        StPureTest.stamped(tables, source);
        new Inferring(StPureTest.alone(temp), temp.resolve("again"), tables).exec();
        MatcherAssert.assertThat(
            "a table rewritten between two transpilations of one JVM must be read again, but the copy parsed first was handed over",
            StPureTest.stamped(tables, source).nodes("//o[@name='x' and @pure='true']"),
            Matchers.empty()
        );
    }

    private Collection<String> unmatched(final Xtory pack) throws IOException {
        final Collection<String> failed = new ArrayList<>(0);
        for (final Object key : pack.map().keySet()) {
            if (!"eo".equals(key) && !"pure".equals(key)) {
                failed.add(String.format("unknown key: %s", key));
            }
        }
        final Path parsed = Files.createDirectories(this.dir.resolve("parsed"));
        final Map<String, String> sources = StPureTest.sources(pack);
        for (final Map.Entry<String, String> source : sources.entrySet()) {
            Files.writeString(
                parsed.resolve(source.getKey()),
                new EoSyntax(source.getValue()).parsed().toString()
            );
        }
        final Path tables = this.dir.resolve("tables");
        new Inferring(parsed, this.dir.resolve("pre"), tables).exec();
        final Collection<XML> stamped = new ArrayList<>(0);
        for (final String name : sources.keySet()) {
            stamped.add(StPureTest.stamped(tables, parsed.resolve(name)));
        }
        for (final String xpath : (List<String>) pack.map().get("pure")) {
            boolean found = false;
            for (final XML xmir : stamped) {
                found = found || !xmir.nodes(xpath).isEmpty();
            }
            if (!found) {
                failed.add(xpath);
            }
        }
        return failed;
    }

    private static Path program(final Path temp) throws IOException {
        final Path parsed = Files.createDirectories(temp.resolve("parsed"));
        Files.writeString(
            parsed.resolve("number.xmir"),
            new EoSyntax(
                String.join(
                    System.lineSeparator(),
                    "[as-bytes] > number", "  as-bytes > @", "  [x] > power", "    x > @", ""
                )
            ).parsed().toString()
        );
        Files.writeString(
            parsed.resolve("app.xmir"),
            new EoSyntax(
                String.join(
                    System.lineSeparator(), "[] > app", "  2.power 63 > x", "  x > @", ""
                )
            ).parsed().toString()
        );
        return parsed;
    }

    private static Path alone(final Path temp) throws IOException {
        final Path parsed = Files.createDirectories(temp.resolve("alone"));
        Files.copy(
            temp.resolve("parsed").resolve("app.xmir"),
            parsed.resolve("app.xmir")
        );
        return parsed;
    }

    private static XML stamped(final Path tables, final Path source) throws IOException {
        return new Xsline(
            new TrDefault<>(
                new StClasspath("/org/eolang/parser/parse/set-locators.xsl"),
                new StPure("/org/eolang/maven/transpile/purify.xsl", tables)
            )
        ).pass(new XMLDocument(source));
    }

    private static Map<String, String> sources(final Xtory pack) {
        final Map<String, String> found = new LinkedHashMap<>(0);
        for (final Map.Entry<?, ?> entry : ((Map<?, ?>) pack.map().get("eo")).entrySet()) {
            found.put(
                entry.getKey().toString().replace(".eo", ".xmir"),
                entry.getValue().toString()
            );
        }
        return found;
    }
}
