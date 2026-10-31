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
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.cactoos.list.ListOf;
import org.cactoos.map.MapEntry;
import org.cactoos.map.MapOf;
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
 * {@code entry-packs}. They say which objects get an entry, what their
 * voids get, and which objects become rows of the table of operations.
 * Every file has an EO program, which the engine of {@code eo:inference}
 * reads to write the tables the planting reads, see {@link Tables}, the
 * XPath queries that the XMIR of the entries and the copies must match,
 * and the rows that {@code entries.tsv}, {@code voids.tsv},
 * {@code rows.tsv} and {@code atoms.yaml} must have.
 * The tests in this class check the things that an EO program cannot
 * show: which files are written, whether two runs give the same result,
 * what the table of operations holds before any row is added, and when
 * the stage refuses to work.</p>
 *
 * @since 0.64.0
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
    void writesTheFilesOfTheBuild(@Mktmp final Path temp) throws IOException {
        final Path home = PlantingTest.planted(temp);
        MatcherAssert.assertThat(
            "the planting must leave the entries, the tables and the table of operations behind, but it didnt",
            new ListOf<>(
                Files.exists(home.resolve("entries.xmir")),
                Files.exists(home.resolve("voids.tsv")),
                Files.exists(home.resolve("entries.tsv")),
                Files.exists(home.resolve("rows.tsv")),
                Files.exists(home.resolve("atoms.yaml"))
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
    void writesTheTableOfOperationsBesideTheWorld(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "the table must name the operations phino may write down, but it doesnt",
            PlantingTest.text(PlantingTest.planted(temp).resolve("atoms.yaml")),
            Matchers.containsString("L_number_gt")
        );
    }

    @Test
    void answersEveryAtomOfBytesInTheTable(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "the table must answer every atom of bytes, but it leaves one standing",
            Stream.of("and", "concat", "eq", "not", "or", "right", "size", "slice")
                .map(atom -> String.format("L_bytes_%s", atom))
                .collect(Collectors.toList()),
            Matchers.everyItem(
                Matchers.matchesPattern(
                    Files.readAllLines(PlantingTest.planted(temp).resolve("atoms.yaml")).stream()
                        .filter(line -> line.startsWith("- λ: "))
                        .map(line -> String.format("(?:%s)", line.substring(5)))
                        .collect(Collectors.joining("|"))
                )
            )
        );
    }

    @Test
    void forksOnTheConditionInsteadOfAnsweringAnIf(@Mktmp final Path temp)
        throws IOException {
        MatcherAssert.assertThat(
            "the fork must read a fabricated copy by the names phino gives it and name itself when a join fails, but it doesnt",
            PlantingTest.text(PlantingTest.planted(temp).resolve("atoms.yaml")),
            Matchers.stringContainsInOrder(
                "- λ: L_fork", "dataize:", "𝛿1: $.on🌵", "morph:", "𝑛1: $.left🌵", "𝑛2: $.right🌵",
                "undress:", "𝑛15: 𝑛9", "𝑛16: 𝑛10", "stay:", "evaluate:", "𝑛17: 𝑛15", "𝑛18: 𝑛16", "rewrite:",
                "symbolize:", "𝑛11: 𝑛17", "𝑛12: 𝑛18", "defer:",
                "cast:", "𝑛19: 𝑛13", "𝑛20: 𝑛14",
                "join:", "'on': 𝛿1", "left: 𝑛19", "right: 𝑛20", "otherwise: L_fork"
            )
        );
    }

    @Test
    void answersEveryBoolAsTheFormationWrappingIt(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "every bool the table answers must be the formation wrapping it, but one is bare",
            Files.readAllLines(PlantingTest.planted(temp).resolve("atoms.yaml")).stream()
                .filter(line -> line.matches("(  𝑛: |    )[^ ].*Φ\\.bool\\(.*"))
                .collect(Collectors.toList()),
            Matchers.<List<String>>allOf(
                Matchers.not(Matchers.empty()),
                Matchers.everyItem(Matchers.matchesRegex("(  𝑛: |    )⟦ φ ↦ Φ\\.bool\\(.*"))
            )
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
    void failsNamingTheAnswersItCannotFind(@Mktmp final Path temp) throws IOException {
        final Path tables = Files.createDirectories(temp.resolve("tables"));
        Files.write(tables.resolve("facts.xml"), "<facts/>".getBytes(StandardCharsets.UTF_8));
        MatcherAssert.assertThat(
            "the failure must name the table that is missing, but it doesnt",
            Assertions.assertThrows(
                IllegalStateException.class,
                () -> new Planting(tables).exec(temp),
                "answers that are not there must fail the planting"
            ).getMessage(),
            Matchers.containsString("answers.xml")
        );
    }

    private static Path planted(final Path temp) throws IOException {
        final Map<String, String> program = new MapOf<String, String>(
            new MapEntry<>("gap.eo", String.format("[a b] > gap%n  a.plus b > @%n")),
            new MapEntry<>("app.eo", String.format("[] > app%n  gap 4 9 > used%n"))
        );
        for (final Map.Entry<String, String> source : program.entrySet()) {
            PlantingTest.parsed(
                Files.createDirectories(temp.resolve("1-planting"))
                    .resolve(source.getKey().replace(".eo", ".xmir")),
                source.getValue()
            );
        }
        new Planting(new Inferred(temp.resolve("engine"), program).value()).exec(temp);
        return temp;
    }

    private static Path parsed(final Path file, final String program) throws IOException {
        return Files.write(
            file, new EoSyntax(program).parsed().toString().getBytes(StandardCharsets.UTF_8)
        );
    }

    private static String text(final Path file) throws IOException {
        return new String(Files.readAllBytes(file), StandardCharsets.UTF_8);
    }

    /**
     * One YAML file of {@code entry-packs}, with what it expects from
     * {@link Planting}.
     *
     * @since 0.64.0
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
                    "eo", "dealpha", "facts", "answers",
                    "xmir", "copies", "voids", "entries", "rows", "table"
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
            final Collection<XMLDocument> copies = new ArrayList<>(0);
            for (final Path copy : new Copies(home)) {
                copies.add(new XMLDocument(copy));
            }
            for (final Object xpath : this.demands("copies")) {
                if (copies.stream().allMatch(copy -> copy.nodes(xpath.toString()).isEmpty())) {
                    failed.add(String.format("copies: %s", xpath));
                }
            }
            failed.addAll(this.missing(home, "voids", "voids.tsv"));
            failed.addAll(this.missing(home, "entries", "entries.tsv"));
            failed.addAll(this.missing(home, "rows", "rows.tsv"));
            failed.addAll(this.missing(home, "table", "atoms.yaml"));
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
            new Planting(new Tables(this.temp, this.story.map()).value()).exec(this.temp);
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

        private Collection<String> missing(final Path home, final String key, final String file)
            throws IOException {
            final Collection<String> failed = new ArrayList<>(0);
            final List<String> rows = Files.readAllLines(
                home.resolve(file), StandardCharsets.UTF_8
            );
            for (final Object row : this.demands(key)) {
                if (!rows.contains(row.toString())) {
                    failed.add(String.format("%s: %s", file, row));
                }
            }
            return failed;
        }

        private List<?> demands(final String key) {
            return (List<?>) this.story.map().getOrDefault(key, new ListOf<>());
        }
    }
}
