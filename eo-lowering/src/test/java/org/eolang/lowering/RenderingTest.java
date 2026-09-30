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
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.cactoos.list.ListOf;
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
 * Tests of the class {@link Rendering}.
 *
 * @since 0.74.0
 */
@ExtendWith(MktmpResolver.class)
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
        final Path home = Files.createDirectories(temp.resolve("7-lowering"));
        Files.write(
            home.resolve("entries.tsv"),
            String.format("3\tΦ.slow%n").getBytes(StandardCharsets.UTF_8)
        );
        Files.write(home.resolve("voids.tsv"), new byte[0]);
        Files.write(
            Files.createDirectories(temp.resolve("7-lowering-planting")).resolve("slow.xmir"),
            new EoSyntax(String.format("[] > slow%n  42 > @%n")).parsed().toString()
                .getBytes(StandardCharsets.UTF_8)
        );
        new Rendering(temp.resolve("atoms")).exec(temp);
        MatcherAssert.assertThat(
            "an entry whose run was killed must be rendered into nothing, but it is",
            Files.exists(temp.resolve("atoms")),
            Matchers.is(false)
        );
    }

    /**
     * One YAML file with an example for {@link Rendering}.
     *
     * <p>The file has EO sources, one entry, its protocol, and the whole
     * text of the Java atom of that entry, which the rendering must write
     * exactly. When the file names no Java file, the entry must be a taint,
     * and no atom may be written.</p>
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
                    "locator", "number", "eo", "voids", "protocol", "file", "java"
                ).contains(key)) {
                    failed.add(String.format("unknown key: %s", key));
                }
            }
            final Path atoms = this.rendered();
            final String listed = Files.readString(
                this.temp.resolve("7-lowering/rendered.tsv"), StandardCharsets.UTF_8
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

        private Path rendered() throws IOException {
            final String locator = this.story.map().get("locator").toString();
            final Path home = Files.createDirectories(this.temp.resolve("7-lowering"));
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
                this.temp.resolve("7-lowering-planting")
            );
            for (final Map.Entry<?, ?> source
                : ((Map<?, ?>) this.story.map().get("eo")).entrySet()) {
                Files.write(
                    sources.resolve(source.getKey().toString().replace(".eo", ".xmir")),
                    new EoSyntax(source.getValue().toString()).parsed().toString()
                        .getBytes(StandardCharsets.UTF_8)
                );
            }
            final Path protocol = this.temp.resolve("7-lowering-protocols")
                .resolve(new Locator(locator).protocol());
            Files.createDirectories(protocol.getParent());
            Files.write(
                protocol,
                this.story.map().get("protocol").toString().getBytes(StandardCharsets.UTF_8)
            );
            new Rendering(this.temp.resolve("atoms")).exec(this.temp);
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
