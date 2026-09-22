/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.jcabi.xml.XMLDocument;
import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import org.cactoos.list.ListOf;
import org.eolang.jucs.ClasspathSource;
import org.eolang.parser.EoSyntax;
import org.eolang.xax.XtSticky;
import org.eolang.xax.XtYaml;
import org.eolang.xax.Xtory;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;

/**
 * Test case for {@link Planting}.
 *
 * <p>Which formation deserves an entry, and what goes into its voids, is
 * said by the packs in {@code entry-packs}: each one carries the program a
 * human would write, the tables {@code eo:inference} would have left about
 * it, the XPaths the planted XMIR must satisfy, and the rows the two
 * tables must hold. What is left here are the mechanics no EO source can
 * express: which files are written, whether two runs agree, and what the
 * stage refuses to do at all.</p>
 *
 * @since 0.74.0
 */
@ExtendWith(MktmpResolver.class)
final class PlantingTest {

    /**
     * Temp directory, injected into every test instance, since a parameterized
     * test cannot also take one as an argument.
     */
    @Mktmp
    private Path dir;

    @ParameterizedTest
    @ClasspathSource(value = "org/eolang/lowering/entry-packs/", glob = "**.yaml")
    void plantsTheEntriesOfAPack(final String yaml) throws IOException {
        MatcherAssert.assertThat(
            "every demand of the pack must be met by what the planting wrote, but some werent",
            new PlantingTest.Pack(new XtSticky(new XtYaml(yaml)), this.dir).unmet(),
            Matchers.empty()
        );
    }

    @Test
    void writesTheThreeFilesOfTheBuild(@Mktmp final Path temp) throws IOException {
        final Path home = PlantingTest.planted(temp);
        MatcherAssert.assertThat(
            "the planting must leave the entries and both tables behind, but it didnt",
            new ListOf<>(
                Files.exists(home.resolve("entries.xmir")),
                Files.exists(home.resolve("voids.tsv")),
                Files.exists(home.resolve("entries.tsv"))
            ),
            Matchers.everyItem(Matchers.is(true))
        );
    }

    @Test
    void plantsTheSameBytesOnEveryRun(@Mktmp final Path temp) throws IOException {
        final Path made = PlantingTest.planted(temp).resolve("entries.xmir");
        final byte[] first = Files.readAllBytes(made);
        PlantingTest.planted(temp);
        MatcherAssert.assertThat(
            "two runs over the same build must plant the very same bytes, but they didnt",
            Files.readAllBytes(made),
            Matchers.equalTo(first)
        );
    }

    @Test
    void failsNamingTheTablesItCannotFind(@Mktmp final Path temp) {
        final Path absent = temp.resolve("tables");
        MatcherAssert.assertThat(
            "the failure must name the directory the tables are missing from, but it doesnt",
            Assertions.assertThrows(
                IllegalStateException.class,
                () -> new Planting(new ListOf<>(), absent, temp.resolve("lower")).exec(),
                "tables that are not there must fail the planting"
            ).getMessage(),
            Matchers.containsString(absent.toString())
        );
    }

    private static Path planted(final Path temp) throws IOException {
        final Path home = temp.resolve("lower");
        new Planting(
            PlantingTest.sources(temp), PlantingTest.tables(temp, "<provides/>"), home
        ).exec();
        return home;
    }

    private static Collection<Path> sources(final Path temp) throws IOException {
        return Collections.singletonList(
            PlantingTest.parsed(
                Files.createDirectories(temp.resolve("sources")).resolve("gap.xmir"),
                String.format("[a b] > gap%n  a.plus b > @%n")
            )
        );
    }

    private static Path tables(final Path temp, final String provides) throws IOException {
        final Path made = Files.createDirectories(temp.resolve("tables"));
        Files.write(made.resolve("provides.xml"), provides.getBytes(StandardCharsets.UTF_8));
        return made;
    }

    private static Path parsed(final Path file, final String program) throws IOException {
        return Files.write(
            file, new EoSyntax(program).parsed().toString().getBytes(StandardCharsets.UTF_8)
        );
    }

    /**
     * One pack of {@code entry-packs}, and what it demands of the planting.
     *
     * @since 0.74.0
     */
    private static final class Pack {

        /**
         * The pack, as it was written.
         */
        private final Xtory story;

        /**
         * The temp directory of the test.
         */
        private final Path temp;

        /**
         * Ctor.
         *
         * @param pack The pack, as it was written
         * @param dir The temp directory of the test
         */
        Pack(final Xtory pack, final Path dir) {
            this.story = pack;
            this.temp = dir;
        }

        /**
         * Every demand of the pack the planting did not meet.
         *
         * @return The demands that were not met, empty when all of them were
         * @throws IOException If anything cannot be read or written
         */
        Collection<String> unmet() throws IOException {
            final Collection<String> failed = new ArrayList<>(0);
            for (final Object key : this.story.map().keySet()) {
                if (!Arrays.asList("eo", "provides", "xmir", "voids", "entries").contains(key)) {
                    failed.add(String.format("unknown key: %s", key));
                }
            }
            final Path home = this.plant();
            final XMLDocument entries = new XMLDocument(home.resolve("entries.xmir"));
            for (final Object xpath : this.demands("xmir")) {
                if (entries.nodes(xpath.toString()).isEmpty()) {
                    failed.add(String.format("entries.xmir: %s", xpath));
                }
            }
            failed.addAll(this.missing(home, "voids"));
            failed.addAll(this.missing(home, "entries"));
            return failed;
        }

        private Path plant() throws IOException {
            final Path sources = Files.createDirectories(this.temp.resolve("sources"));
            final Collection<Path> paths = new ArrayList<>(0);
            for (final Map.Entry<?, ?> source
                : ((Map<?, ?>) this.story.map().get("eo")).entrySet()) {
                paths.add(
                    PlantingTest.parsed(
                        sources.resolve(source.getKey().toString().replace(".eo", ".xmir")),
                        source.getValue().toString()
                    )
                );
            }
            final Path home = this.temp.resolve("lower");
            new Planting(
                paths,
                PlantingTest.tables(
                    this.temp,
                    this.story.map().getOrDefault("provides", "<provides/>").toString()
                ),
                home
            ).exec();
            return home;
        }

        private Collection<String> missing(final Path home, final String name)
            throws IOException {
            final Collection<String> failed = new ArrayList<>(0);
            final List<String> rows = Files.readAllLines(
                home.resolve(String.format("%s.tsv", name)), StandardCharsets.UTF_8
            );
            for (final Object row : this.demands(name)) {
                if (!rows.contains(row.toString())) {
                    failed.add(String.format("%s.tsv: %s", name, row));
                }
            }
            return failed;
        }

        private List<?> demands(final String key) {
            return (List<?>) this.story.map().getOrDefault(key, new ListOf<>());
        }
    }
}
