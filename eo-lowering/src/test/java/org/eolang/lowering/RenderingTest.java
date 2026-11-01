/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

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
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.apache.log4j.AppenderSkeleton;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.apache.log4j.spi.LoggingEvent;
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

/**
 * Tests of the class {@link Rendering}.
 *
 * @since 0.64.0
 */
@ExtendWith(MktmpResolver.class)
@Isolated
final class RenderingTest {

    /**
     * A temporary directory for the test. It is a field, and not an argument
     * of the test method, because a parameterized test cannot take it as an
     * argument.
     */
    @Mktmp
    private Path dir;

    @ParameterizedTest
    @ClasspathSource(value = "org/eolang/lowering/render-packs/", glob = "**.yaml")
    void rendersTheAtomsOfAPack(final String yaml) throws IOException {
        MatcherAssert.assertThat(
            "every demand of the pack must be met by what the rendering wrote, but some werent",
            new RenderingTest.Pack(new XtSticky(new XtYaml(yaml)), this.dir).unmet(),
            Matchers.empty()
        );
    }

    @Test
    void rendersNothingOfAnEntryWithNoProtocol(@Mktmp final Path temp) throws IOException {
        final Path home = Files.createDirectories(temp);
        Files.write(
            home.resolve("entries.tsv"),
            String.format("3\tΦ.slow%n").getBytes(StandardCharsets.UTF_8)
        );
        Files.write(home.resolve("voids.tsv"), new byte[0]);
        Files.createDirectories(temp.resolve("2-protocols"));
        new Rendering(
            RenderingTest.planted(temp, "slow", String.format("[] > slow%n  42 > @%n"))
        ).exec(temp);
        MatcherAssert.assertThat(
            "an entry whose run was killed must be rendered into nothing, but it is",
            temp.resolve("3-atoms").toFile().list(),
            Matchers.emptyArray()
        );
    }

    @Test
    void removesTheAtomsOfAnEarlierBuild(@Mktmp final Path temp) throws IOException {
        final Path home = Files.createDirectories(temp);
        Files.write(
            home.resolve("entries.tsv"),
            String.format("3\tΦ.slow%n").getBytes(StandardCharsets.UTF_8)
        );
        Files.write(home.resolve("voids.tsv"), new byte[0]);
        Files.createDirectories(temp.resolve("2-protocols"));
        final Path tables = RenderingTest.planted(
            temp, "slow", String.format("[] > slow%n  42 > @%n")
        );
        final Path stale = Files.createDirectories(
            temp.resolve("3-atoms").resolve("org").resolve("eolang")
        ).resolve("EOstale$EOφ.java");
        Files.write(stale, "// nothing".getBytes(StandardCharsets.UTF_8));
        new Rendering(tables).exec(temp);
        MatcherAssert.assertThat(
            "an atom of an earlier build must not survive the rendering, but it did",
            Files.exists(stale),
            Matchers.is(false)
        );
    }

    @Test
    void rendersNothingOfTwoEntriesWhoseAtomsShareAClass(@Mktmp final Path temp)
        throws IOException {
        Files.write(
            temp.resolve("entries.tsv"),
            String.format("1\tΦ.foo.φ.α0%n2\tΦ.foo.φ.α1%n").getBytes(StandardCharsets.UTF_8)
        );
        Files.write(
            temp.resolve("voids.tsv"),
            String.format("𝜎1\t1\tx\tobject%n𝜎2\t2\ty\tobject%n").getBytes(StandardCharsets.UTF_8)
        );
        final Path tables = RenderingTest.planted(
            temp,
            "foo",
            String.format("[] > foo%n  bar > @%n    [x]%n      x > @%n    [y]%n      y > @%n")
        );
        for (final int idx : new ListOf<>(0, 1)) {
            final Path protocol = temp.resolve("2-protocols").resolve(
                new Locator(String.format("Φ.foo.φ.α%d", idx)).protocol()
            );
            Files.createDirectories(protocol.getParent());
            Files.write(
                protocol,
                String.format(
                    "<protocol><dataize at=\"Φ.l🌵.e1\"><manufactured meta=\"𝛿·0·1\">𝜎%d</manufactured></dataize></protocol>",
                    idx + 1
                ).getBytes(StandardCharsets.UTF_8)
            );
        }
        new Rendering(tables).exec(temp);
        MatcherAssert.assertThat(
            "two entries whose atoms ask for one class must both be taints, but some were rendered",
            Files.readString(temp.resolve("rendered.tsv"), StandardCharsets.UTF_8),
            Matchers.emptyString()
        );
    }

    @Test
    void failsNamingTheHomeWithoutProtocols(@Mktmp final Path temp) throws IOException {
        Files.write(
            Files.createDirectories(temp).resolve("entries.tsv"),
            String.format("5\tΦ.idle%n").getBytes(StandardCharsets.UTF_8)
        );
        final Path tables = RenderingTest.planted(
            temp, "idle", String.format("[] > idle%n  42 > @%n")
        );
        MatcherAssert.assertThat(
            "the failure must name the home with no protocols, but it doesnt",
            Assertions.assertThrows(
                IllegalStateException.class,
                () -> new Rendering(tables).exec(temp),
                "a home without protocols must fail the rendering"
            ).getMessage(),
            Matchers.containsString(temp.toString())
        );
    }

    @Test
    void logsAnEntryWithNoProtocol(@Mktmp final Path temp) throws IOException {
        final Path home = Files.createDirectories(temp);
        final int number = new SecureRandom().nextInt(900) + 100;
        Files.write(
            home.resolve("entries.tsv"),
            String.format("%d\tΦ.lazy%n", number).getBytes(StandardCharsets.UTF_8)
        );
        Files.write(home.resolve("voids.tsv"), new byte[0]);
        Files.createDirectories(temp.resolve("2-protocols"));
        MatcherAssert.assertThat(
            "an entry with no protocol must be logged as left in EO, but it isnt",
            RenderingTest.logged(
                new Rendering(
                    RenderingTest.planted(temp, "lazy", String.format("[] > lazy%n  42 > @%n"))
                ),
                temp
            ),
            Matchers.hasItem(
                Matchers.allOf(
                    Matchers.containsString(String.format("entry %d at Φ.lazy", number)),
                    Matchers.containsString("has no protocol")
                )
            )
        );
    }

    @Test
    void namesTheTaintsInTheLog(@Mktmp final Path temp) throws IOException {
        final int count = new SecureRandom().nextInt(4) + 2;
        final List<String> names = new ArrayList<>(count);
        final Map<String, String> eos = new HashMap<>(count);
        final StringBuilder entries = new StringBuilder();
        for (int idx = 0; idx < count; ++idx) {
            final String name = String.format("zj%d", new SecureRandom().nextInt(1000) * 10 + idx);
            final String program = String.format("[] > %s%n  42 > @%n", name);
            names.add(name);
            eos.put(String.format("%s.eo", name), program);
            entries.append(String.format("%d\tΦ.%s%n", idx, name));
            Files.write(
                Files.createDirectories(temp.resolve("1-planting"))
                    .resolve(String.format("%s.xmir", name)),
                new EoSyntax(program).parsed().toString().getBytes(StandardCharsets.UTF_8)
            );
            final Path protocol = temp.resolve("2-protocols")
                .resolve(new Locator(String.format("Φ.%s", name)).protocol());
            Files.createDirectories(protocol.getParent());
            Files.write(protocol, "<protocol/>".getBytes(StandardCharsets.UTF_8));
        }
        Files.write(
            temp.resolve("entries.tsv"), entries.toString().getBytes(StandardCharsets.UTF_8)
        );
        Files.write(temp.resolve("voids.tsv"), new byte[0]);
        MatcherAssert.assertThat(
            "the log must name the entries that are taints, but it doesnt",
            RenderingTest.logged(
                new Rendering(new Inferred(temp.resolve("engine"), eos).value()),
                temp
            ),
            Matchers.hasItem(
                Matchers.endsWith(
                    String.format(
                        "while %d entries were taints: %s", count, String.join(", ", names)
                    )
                )
            )
        );
    }

    @Test
    void failsNamingTheAnswersItCannotFind(@Mktmp final Path temp) throws IOException {
        final Path tables = Files.createDirectories(temp.resolve("tables"));
        Files.write(tables.resolve("facts.xml"), "<facts/>".getBytes(StandardCharsets.UTF_8));
        MatcherAssert.assertThat(
            "the failure must name the answers that are missing, but it doesnt",
            Assertions.assertThrows(
                IllegalStateException.class,
                () -> new Rendering(tables).exec(temp),
                "tables without the answers must fail the rendering"
            ).getMessage(),
            Matchers.containsString("answers.xml")
        );
    }

    private static Path planted(final Path temp, final String name, final String program)
        throws IOException {
        Files.write(
            Files.createDirectories(temp.resolve("1-planting"))
                .resolve(String.format("%s.xmir", name)),
            new EoSyntax(program).parsed().toString().getBytes(StandardCharsets.UTF_8)
        );
        return new Inferred(
            temp.resolve("engine"),
            new MapOf<String, String>(new MapEntry<>(String.format("%s.eo", name), program))
        ).value();
    }

    private static List<String> logged(final Rendering rendering, final Path home)
        throws IOException {
        final List<String> messages = new ArrayList<>(0);
        final AppenderSkeleton appender = new AppenderSkeleton() {
            @Override
            protected void append(final LoggingEvent event) {
                if (event.getLevel().equals(Level.INFO)) {
                    messages.add(String.valueOf(event.getRenderedMessage()));
                }
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
        final Logger logger = Logger.getLogger(Rendering.class);
        final Level level = logger.getLevel();
        logger.setLevel(Level.INFO);
        logger.addAppender(appender);
        try {
            rendering.exec(home);
        } finally {
            logger.removeAppender(appender);
            logger.setLevel(level);
        }
        return messages;
    }

    /**
     * One YAML file with an example for {@link Rendering}.
     *
     * <p>The file has EO sources, one entry, its protocol, and the whole
     * text of the Java atom of that entry, which the rendering must write
     * exactly. The tables of {@code eo:inference} are what the engine writes
     * for the sources, unless the keys {@code facts} and {@code answers}
     * hold them, see {@link Tables}. So the sources hold what the engine
     * needs to tell what the body gives, the atoms it calls and a caller
     * that hands its voids their data, besides the object of the entry.
     * When the file names no Java file, the entry must be a taint, and no
     * atom may be written. Then the key {@code taint} may hold a part of the
     * line the log must have about it, which says why.</p>
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
         * @param home The temporary directory of the test
         */
        Pack(final Xtory pack, final Path home) {
            this.story = pack;
            this.temp = home;
        }

        /**
         * Every check of the YAML file that failed after the rendering.
         *
         * @return The checks that failed, or an empty list when all of them passed
         * @throws IOException If a file cannot be read or written
         */
        Collection<String> unmet() throws IOException {
            final Collection<String> failed = new ArrayList<>(0);
            for (final Object key : this.story.map().keySet()) {
                if (!Arrays.asList(
                    "locator", "number", "eo", "voids", "rows", "facts", "answers",
                    "protocol", "file", "java", "taint"
                ).contains(key)) {
                    failed.add(String.format("unknown key: %s", key));
                }
            }
            final List<String> log = new ArrayList<>(0);
            final Path atoms = this.rendered(log);
            final String listed = Files.readString(
                this.temp.resolve("rendered.tsv"), StandardCharsets.UTF_8
            );
            final String row = String.format(
                "%s\t%s%n", this.story.map().get("number"), this.story.map().get("locator")
            );
            if (this.story.map().containsKey("file")) {
                failed.addAll(this.missing(atoms));
                if (!listed.equals(row)) {
                    failed.add(String.format("rendered.tsv: %s, while %s", row, listed));
                }
            } else if (!this.files(atoms).isEmpty() || !listed.isEmpty()) {
                failed.add(String.format("no file, while %s and %s", this.files(atoms), listed));
            }
            if (this.story.map().containsKey("taint")) {
                final String why = this.story.map().get("taint").toString();
                if (log.stream().noneMatch(line -> line.contains(why))) {
                    failed.add(String.format("taint: '%s' is not in the log %s", why, log));
                }
            }
            return failed;
        }

        private Collection<String> missing(final Path atoms) throws IOException {
            final Collection<String> failed = new ArrayList<>(0);
            final Path file = atoms.resolve(this.story.map().get("file").toString());
            if (Files.exists(file)) {
                final String java = Files.readString(file, StandardCharsets.UTF_8);
                if (!java.equals(this.story.map().get("java"))) {
                    failed.add(String.format("java differs, the rendering wrote:%n%s", java));
                }
            } else {
                failed.add(String.format("file: %s, only %s", file, this.files(atoms)));
            }
            return failed;
        }

        private Path rendered(final List<String> log) throws IOException {
            final String locator = this.story.map().get("locator").toString();
            final Path home = Files.createDirectories(this.temp);
            Files.write(
                home.resolve("entries.tsv"),
                String.format("%s\t%s%n", this.story.map().get("number"), locator)
                    .getBytes(StandardCharsets.UTF_8)
            );
            for (final String table : new ListOf<>("voids", "rows")) {
                Files.write(
                    home.resolve(String.format("%s.tsv", table)),
                    this.demands(table).stream()
                        .map(row -> String.format("%s%n", row))
                        .collect(Collectors.joining())
                        .getBytes(StandardCharsets.UTF_8)
                );
            }
            final Path sources = Files.createDirectories(
                this.temp.resolve("1-planting")
            );
            for (final Map.Entry<?, ?> source
                : ((Map<?, ?>) this.story.map().get("eo")).entrySet()) {
                Files.write(
                    sources.resolve(source.getKey().toString().replace(".eo", ".xmir")),
                    new EoSyntax(source.getValue().toString()).parsed().toString()
                        .getBytes(StandardCharsets.UTF_8)
                );
            }
            final Path protocol = this.temp.resolve("5-protocols")
                .resolve(new Locator(locator).protocol());
            Files.createDirectories(protocol.getParent());
            Files.write(
                protocol,
                this.story.map().get("protocol").toString().getBytes(StandardCharsets.UTF_8)
            );
            log.addAll(
                RenderingTest.logged(
                    new Rendering(
                        new Tables(this.temp, this.story.map()).value()
                    ),
                    this.temp
                )
            );
            return this.temp.resolve("6-atoms");
        }

        private List<Path> files(final Path atoms) throws IOException {
            final List<Path> files;
            if (Files.exists(atoms)) {
                try (Stream<Path> all = Files.walk(atoms)) {
                    files = all.filter(Files::isRegularFile)
                        .map(atoms::relativize)
                        .collect(Collectors.toList());
                }
            } else {
                files = new ListOf<>();
            }
            return files;
        }

        private List<?> demands(final String key) {
            return (List<?>) this.story.map().getOrDefault(key, new ListOf<>());
        }
    }
}
