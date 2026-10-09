/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import com.yegor256.xsline.Shift;
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
 * <p>The other half of the label is here as well: what {@code to-java.xsl}
 * makes of an object the stylesheet marked. Such an object has to come out of
 * the transpiler wrapped in {@code PhSticky}, so that the bytes it works out
 * are remembered instead of being worked out on every read of it (see
 * #5165).</p>
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
    void wrapsApplicationOfDataInPhSticky(@Mktmp final Path temp) throws IOException {
        final Path parsed = Files.createDirectories(temp.resolve("parsed"));
        Files.writeString(
            parsed.resolve("app.xmir"),
            new EoSyntax(
                String.join(
                    System.lineSeparator(),
                    "[] > app", "  2.plus 3 > x", "  x > @", ""
                )
            ).parsed().toString()
        );
        Files.writeString(
            parsed.resolve("number.xmir"),
            new EoSyntax(
                String.join(
                    System.lineSeparator(),
                    "[as-bytes] > number", "  as-bytes > @",
                    "  [x] > plus", "    x > @", ""
                )
            ).parsed().toString()
        );
        final Path tables = temp.resolve("tables");
        new Inferring(parsed, temp.resolve("pre"), tables).exec();
        MatcherAssert.assertThat(
            "an application whose parts are all data must be wrapped in PhSticky, but it wasnt",
            new Xsline(
                new TrDefault<Shift>()
                    .with(new StClasspath("/org/eolang/parser/parse/set-locators.xsl"))
                    .with(new StClasspath("/org/eolang/maven/transpile/set-original-names.xsl"))
                    .with(new StClasspath("/org/eolang/maven/transpile/classes.xsl"))
                    .with(new StClasspath("/org/eolang/maven/transpile/attrs.xsl"))
                    .with(new StClasspath("/org/eolang/maven/transpile/data.xsl"))
                    .with(new StPure("/org/eolang/maven/transpile/purify.xsl", tables))
                    .with(new StClasspath("/org/eolang/maven/transpile/to-java.xsl"))
            ).pass(new XMLDocument(parsed.resolve("app.xmir"))).toString(),
            Matchers.containsString("new PhSticky(new PhApplication(")
        );
    }

    @Test
    void wrapsDispatchWithoutArgumentsInPhSticky(@Mktmp final Path temp) throws IOException {
        final Path parsed = Files.createDirectories(temp.resolve("parsed"));
        Files.writeString(
            parsed.resolve("app.xmir"),
            new EoSyntax(
                String.join(
                    System.lineSeparator(),
                    "[] > app", "  2.neg > x", "  x > @", ""
                )
            ).parsed().toString()
        );
        Files.writeString(
            parsed.resolve("number.xmir"),
            new EoSyntax(
                String.join(
                    System.lineSeparator(),
                    "[as-bytes] > number", "  as-bytes > @",
                    "  [] > neg", "    as-bytes > @", ""
                )
            ).parsed().toString()
        );
        final Path tables = temp.resolve("tables");
        new Inferring(parsed, temp.resolve("pre"), tables).exec();
        MatcherAssert.assertThat(
            "a dispatch over data that takes no arguments must be wrapped in PhSticky, but it wasnt",
            new Xsline(
                new TrDefault<Shift>()
                    .with(new StClasspath("/org/eolang/parser/parse/set-locators.xsl"))
                    .with(new StClasspath("/org/eolang/maven/transpile/set-original-names.xsl"))
                    .with(new StClasspath("/org/eolang/maven/transpile/classes.xsl"))
                    .with(new StClasspath("/org/eolang/maven/transpile/attrs.xsl"))
                    .with(new StClasspath("/org/eolang/maven/transpile/data.xsl"))
                    .with(new StPure("/org/eolang/maven/transpile/purify.xsl", tables))
                    .with(new StClasspath("/org/eolang/maven/transpile/to-java.xsl"))
            ).pass(new XMLDocument(parsed.resolve("app.xmir"))).toString(),
            Matchers.containsString("new PhSticky(")
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
