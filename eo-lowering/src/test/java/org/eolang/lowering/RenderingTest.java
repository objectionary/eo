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
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;
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
        Files.write(
            Files.createDirectories(temp.resolve("1-planting")).resolve("slow.xmir"),
            new EoSyntax(String.format("[] > slow%n  42 > @%n")).parsed().toString()
                .getBytes(StandardCharsets.UTF_8)
        );
        new Rendering(
            temp.resolve("atoms"), RenderingTest.tables(temp, "<links/>", "<atoms/>", "<provides/>")
        ).exec(temp);
        MatcherAssert.assertThat(
            "an entry whose run was killed must be rendered into nothing, but it is",
            Files.exists(temp.resolve("atoms")),
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
        Files.write(
            Files.createDirectories(temp.resolve("1-planting")).resolve("foo.xmir"),
            new EoSyntax(
                String.format("[] > foo%n  bar > @%n    [x]%n      x > @%n    [y]%n      y > @%n")
            ).parsed().toString().getBytes(StandardCharsets.UTF_8)
        );
        for (final int idx : new ListOf<>(0, 1)) {
            final Path protocol = temp.resolve("2-protocols").resolve(
                new Locator(String.format("Φ.foo.φ.α%d", idx)).protocol()
            );
            Files.createDirectories(protocol.getParent());
            Files.write(
                protocol,
                String.format(
                    "<protocol><morph><evaluate λ=\"L_root\"><dataize meta=\"𝛿1.2\">𝜎%d:λ</dataize></evaluate></morph></protocol>",
                    idx + 1
                ).getBytes(StandardCharsets.UTF_8)
            );
        }
        new Rendering(
            temp.resolve("atoms"), RenderingTest.tables(temp, "<links/>", "<atoms/>", "<provides/>")
        ).exec(temp);
        MatcherAssert.assertThat(
            "two entries whose atoms ask for one class must both be taints, but some were rendered",
            Files.readString(temp.resolve("rendered.tsv"), StandardCharsets.UTF_8),
            Matchers.emptyString()
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
        Files.write(
            Files.createDirectories(temp.resolve("1-planting")).resolve("lazy.xmir"),
            new EoSyntax(String.format("[] > lazy%n  42 > @%n")).parsed().toString()
                .getBytes(StandardCharsets.UTF_8)
        );
        MatcherAssert.assertThat(
            "an entry with no protocol must be logged as left in EO, but it isnt",
            RenderingTest.logged(
                new Rendering(
                    temp.resolve("atoms"),
                    RenderingTest.tables(temp, "<links/>", "<atoms/>", "<provides/>")
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
    void failsNamingTheTableOfTheBodiesItCannotFind(@Mktmp final Path temp) throws IOException {
        final Path tables = Files.createDirectories(temp.resolve("tables"));
        Files.write(tables.resolve("provides.xml"), "<provides/>".getBytes(StandardCharsets.UTF_8));
        MatcherAssert.assertThat(
            "the failure must name the table of the bodies that is missing, but it doesnt",
            Assertions.assertThrows(
                IllegalStateException.class,
                () -> new Rendering(temp.resolve("atoms"), tables).exec(temp),
                "tables without the types of the bodies must fail the rendering"
            ).getMessage(),
            Matchers.containsString("links.xml")
        );
    }

    private static Path tables(
        final Path temp, final String links, final String atoms, final String provides
    ) throws IOException {
        final Path made = Files.createDirectories(temp.resolve("tables"));
        Files.write(made.resolve("links.xml"), links.getBytes(StandardCharsets.UTF_8));
        Files.write(made.resolve("atoms.xml"), atoms.getBytes(StandardCharsets.UTF_8));
        Files.write(made.resolve("provides.xml"), provides.getBytes(StandardCharsets.UTF_8));
        return made;
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
     * exactly. The keys {@code links}, {@code atoms} and {@code provides}
     * may hold the tables of {@code eo:inference}, which are empty
     * otherwise. When the file names no Java file, the entry must be a
     * taint, and no atom may be written. Then the key {@code taint} may
     * hold a part of the line the log must have about it, which says
     * why.</p>
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
                    "locator", "number", "eo", "voids", "links", "atoms", "provides",
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
            Files.write(
                home.resolve("voids.tsv"),
                this.demands("voids").stream()
                    .map(row -> String.format("%s%n", row))
                    .collect(Collectors.joining())
                    .getBytes(StandardCharsets.UTF_8)
            );
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
            final Path protocol = this.temp.resolve("2-protocols")
                .resolve(new Locator(locator).protocol());
            Files.createDirectories(protocol.getParent());
            Files.write(
                protocol,
                this.story.map().get("protocol").toString().getBytes(StandardCharsets.UTF_8)
            );
            log.addAll(
                RenderingTest.logged(
                    new Rendering(
                        this.temp.resolve("atoms"),
                        RenderingTest.tables(
                            this.temp,
                            this.story.map().getOrDefault("links", "<links/>").toString(),
                            this.story.map().getOrDefault("atoms", "<atoms/>").toString(),
                            this.story.map().getOrDefault("provides", "<provides/>").toString()
                        )
                    ),
                    this.temp
                )
            );
            return this.temp.resolve("atoms");
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
