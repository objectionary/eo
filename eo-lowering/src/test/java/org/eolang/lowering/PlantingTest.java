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
import java.security.SecureRandom;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.log4j.AppenderSkeleton;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.apache.log4j.spi.LoggingEvent;
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
import org.junit.jupiter.api.parallel.Isolated;
import org.junit.jupiter.params.ParameterizedTest;
import org.xembly.Directives;
import org.xembly.Xembler;

/**
 * Tests of the class {@link Planting}.
 *
 * <p>Most of the checks are in the YAML files of the directory
 * {@code entry-packs}. They say which objects get an entry, and what their
 * voids get. Every file has an EO program, the tables that
 * {@code eo:inference} would write for it, the XPath queries that the
 * XMIR of the entries must match, and the rows that {@code entries.tsv}
 * and {@code voids.tsv} must have. The tests in this class check the
 * things that an EO program cannot show: which files are written, whether
 * two runs give the same result, and when the stage refuses to work.</p>
 *
 * @since 0.74.0
 */
@ExtendWith(MktmpResolver.class)
@Isolated
final class PlantingTest {

    /**
     * A temporary directory for the test. It is a field, and not an argument
     * of the test method, because a parameterized test cannot take it as an
     * argument.
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
                () -> new Planting(absent).exec(temp),
                "tables that are not there must fail the planting"
            ).getMessage(),
            Matchers.containsString(absent.toString())
        );
    }

    @Test
    void failsNamingTheTableOfTheBodiesItCannotFind(@Mktmp final Path temp) throws IOException {
        final Path tables = Files.createDirectories(temp.resolve("tables"));
        Files.write(tables.resolve("provides.xml"), "<provides/>".getBytes(StandardCharsets.UTF_8));
        MatcherAssert.assertThat(
            "the failure must name the table of the bodies that is missing, but it doesnt",
            Assertions.assertThrows(
                IllegalStateException.class,
                () -> new Planting(tables).exec(temp),
                "tables without the types of the bodies must fail the planting"
            ).getMessage(),
            Matchers.containsString("links.xml")
        );
    }

    @Test
    void countsTheFormationsLeftOutForGivingAnotherType(@Mktmp final Path temp)
        throws IOException {
        final int count = new SecureRandom().nextInt(5) + 2;
        final StringBuilder program = new StringBuilder();
        final StringBuilder links = new StringBuilder("<links>");
        for (int idx = 0; idx < count; ++idx) {
            program.append(String.format("[w] > kq%d%n  w.as-i16 > @%n%n", idx));
            links.append(
                String.format("<type id='Φ.kq%d.φ'><ref loc='Φ.i16'/></type>", idx)
            );
        }
        PlantingTest.parsed(
            Files.createDirectories(temp.resolve("1-planting")).resolve("kq.xmir"),
            program.toString()
        );
        MatcherAssert.assertThat(
            "the log must count the formations that give another type, but it doesnt",
            PlantingTest.logged(
                new Planting(
                    PlantingTest.tables(
                        temp, "<provides/>", links.append("</links>").toString(), "<atoms/>"
                    )
                ),
                temp
            ),
            Matchers.hasItem(Matchers.containsString(String.format("%d of other types", count)))
        );
    }

    @Test
    void countsTheFormationsLeftOutForAnUnknownType(@Mktmp final Path temp)
        throws IOException {
        final int count = new SecureRandom().nextInt(5) + 2;
        final StringBuilder program = new StringBuilder();
        for (int idx = 0; idx < count; ++idx) {
            program.append(String.format("[j] > rv%d%n  j.plus 3 > @%n%n", idx));
        }
        PlantingTest.parsed(
            Files.createDirectories(temp.resolve("1-planting")).resolve("rv.xmir"),
            program.toString()
        );
        MatcherAssert.assertThat(
            "the log must count the formations of an unknown type, but it doesnt",
            PlantingTest.logged(
                new Planting(PlantingTest.tables(temp, "<provides/>", "<links/>", "<atoms/>")),
                temp
            ),
            Matchers.hasItem(Matchers.containsString(String.format("%d of unknown type", count)))
        );
    }

    private static List<String> logged(final Planting planting, final Path home)
        throws IOException {
        final List<String> messages = new ArrayList<>(0);
        final AppenderSkeleton appender = new AppenderSkeleton() {
            @Override
            protected void append(final LoggingEvent event) {
                messages.add(String.valueOf(event.getRenderedMessage()));
            }

            @Override
            public void close() {
                // Nothing to release.
            }

            @Override
            public boolean requiresLayout() {
                return false;
            }
        };
        final Logger logger = Logger.getLogger(Planting.class);
        final Level level = logger.getLevel();
        logger.setLevel(Level.INFO);
        logger.addAppender(appender);
        try {
            planting.exec(home);
        } finally {
            logger.removeAppender(appender);
            logger.setLevel(level);
        }
        return messages;
    }

    private static Path planted(final Path temp) throws IOException {
        PlantingTest.parsed(
            Files.createDirectories(temp.resolve("1-planting")).resolve("gap.xmir"),
            String.format("[a b] > gap%n  a.plus b > @%n")
        );
        new Planting(
            PlantingTest.tables(
                temp,
                "<provides/>",
                "<links><type id='Φ.gap.φ'><ref loc='Φ.number'/></type></links>",
                "<atoms/>"
            )
        ).exec(temp);
        return temp;
    }

    private static Path tables(final Path temp, final String provides, final String links,
        final String atoms) throws IOException {
        final Path made = Files.createDirectories(temp.resolve("tables"));
        Files.write(made.resolve("provides.xml"), provides.getBytes(StandardCharsets.UTF_8));
        Files.write(made.resolve("links.xml"), links.getBytes(StandardCharsets.UTF_8));
        Files.write(made.resolve("atoms.xml"), atoms.getBytes(StandardCharsets.UTF_8));
        return made;
    }

    private static Path parsed(final Path file, final String program) throws IOException {
        return Files.write(
            file, new EoSyntax(program).parsed().toString().getBytes(StandardCharsets.UTF_8)
        );
    }

    /**
     * One YAML file of {@code entry-packs}, with what it expects from
     * {@link Planting}.
     *
     * @since 0.74.0
     */
    private static final class Pack {

        /**
         * The content of the YAML file.
         */
        private final Xtory story;

        /**
         * The temporary directory of the test.
         */
        private final Path temp;

        /**
         * Ctor.
         *
         * @param pack The content of the YAML file
         * @param dir The temporary directory of the test
         */
        Pack(final Xtory pack, final Path dir) {
            this.story = pack;
            this.temp = dir;
        }

        /**
         * Every check of the YAML file that failed after the planting.
         *
         * @return The checks that failed, or an empty list when all of them passed
         * @throws IOException If a file cannot be read or written
         */
        Collection<String> unmet() throws IOException {
            final Collection<String> failed = new ArrayList<>(0);
            for (final Object key : this.story.map().keySet()) {
                if (!Arrays.asList(
                    "eo", "dealpha", "provides", "links", "atoms", "xmir", "voids", "entries"
                ).contains(key)) {
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
            final Path sources = Files.createDirectories(
                this.temp.resolve("1-planting")
            );
            for (final Map.Entry<?, ?> source
                : ((Map<?, ?>) this.story.map().get("eo")).entrySet()) {
                this.renamed(
                    PlantingTest.parsed(
                        sources.resolve(source.getKey().toString().replace(".eo", ".xmir")),
                        source.getValue().toString()
                    )
                );
            }
            new Planting(
                PlantingTest.tables(
                    this.temp,
                    this.story.map().getOrDefault("provides", "<provides/>").toString(),
                    this.story.map().getOrDefault("links", "<links/>").toString(),
                    this.story.map().getOrDefault("atoms", "<atoms/>").toString()
                )
            ).exec(this.temp);
            return this.temp;
        }

        private Path renamed(final Path xmir) throws IOException {
            final Directives dirs = new Directives();
            for (final Map.Entry<?, ?> arg
                : ((Map<?, ?>) this.story.map().getOrDefault("dealpha", new HashMap<>(0)))
                .entrySet()) {
                dirs.xpath(String.format("//o[@loc='%s']", arg.getKey()))
                    .attr("as", arg.getValue().toString());
            }
            Files.write(
                xmir,
                new XMLDocument(new Xembler(dirs).applyQuietly(new XMLDocument(xmir).inner()))
                    .toString().getBytes(StandardCharsets.UTF_8)
            );
            return xmir;
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
