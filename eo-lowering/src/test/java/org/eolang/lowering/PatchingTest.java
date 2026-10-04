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
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.TreeSet;
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
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;

/**
 * Tests of the class {@link Patching}.
 *
 * @since 0.64.0
 */
@ExtendWith(MktmpResolver.class)
final class PatchingTest {

    /**
     * A temporary directory for the test. It is a field, and not an argument
     * of the test method, because a parameterized test cannot take it as an
     * argument.
     */
    @Mktmp
    private Path dir;

    @ParameterizedTest
    @ClasspathSource(value = "org/eolang/lowering/patch-packs/", glob = "**.yaml")
    void patchesTheSourcesOfAPack(final String yaml) throws IOException {
        MatcherAssert.assertThat(
            "every demand of the pack must be met by what the patching wrote, but some werent",
            new PatchingTest.Pack(new XtSticky(new XtYaml(yaml)), this.dir).unmet(),
            Matchers.empty()
        );
    }

    @Test
    void failsNamingTheListOfTheRenderedItCannotFind(@Mktmp final Path temp) {
        MatcherAssert.assertThat(
            "the failure must name the list of the rendered entries, but it doesnt",
            Assertions.assertThrows(
                IllegalStateException.class,
                () -> new Patching(
                    new ListOf<>(), temp.resolve("tables"), temp.resolve("patched")
                ).exec(temp),
                "a patching with no list of the rendered entries must fail"
            ).getMessage(),
            Matchers.containsString("rendered.tsv")
        );
    }

    @Test
    void leavesTheCopyOfAnEarlierBuildOutOfTheList(@Mktmp final Path temp) throws IOException {
        Files.write(
            Files.createDirectories(temp).resolve("rendered.tsv"),
            new byte[0]
        );
        Files.write(
            Files.createDirectories(temp.resolve("patched")).resolve("gap.xmir"),
            "<object/>".getBytes(StandardCharsets.UTF_8)
        );
        new Patching(
            Collections.singletonList(
                Files.write(
                    temp.resolve("gap.xmir"),
                    new EoSyntax(String.format("[a b] > gap%n  a.plus b > @%n")).parsed()
                        .toString().getBytes(StandardCharsets.UTF_8)
                )
            ),
            temp.resolve("tables"),
            temp.resolve("patched")
        ).exec(temp);
        MatcherAssert.assertThat(
            "a copy an earlier build patched must not be listed as patched now, but it is",
            Files.readString(temp.resolve("patched.tsv"), StandardCharsets.UTF_8),
            Matchers.emptyString()
        );
    }

    /**
     * One YAML file with an example for {@link Patching}.
     *
     * <p>The file has EO sources, the list of the entries that were turned
     * into atoms, and, for every source that must be patched, the XPath
     * queries that the patched XMIR must match. A source that is not
     * listed must not be patched at all.</p>
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
         * Every check of the YAML file that failed after the patching.
         *
         * @return The checks that failed, or an empty list when all of them passed
         * @throws IOException If a file cannot be read or written
         */
        Collection<String> unmet() throws IOException {
            final Collection<String> failed = new ArrayList<>(0);
            for (final Object key : this.story.map().keySet()) {
                if (!Arrays.asList("eo", "rendered", "patched").contains(key)) {
                    failed.add(String.format("unknown key: %s", key));
                }
            }
            final Path patched = this.patched();
            final Map<?, ?> demands = (Map<?, ?>) this.story.map().get("patched");
            for (final Path file : this.files(patched)) {
                if (!demands.containsKey(file.toString())) {
                    failed.add(String.format("%s is patched, while it must not be", file));
                }
            }
            for (final Map.Entry<?, ?> demand : demands.entrySet()) {
                failed.addAll(
                    PatchingTest.Pack.missing(
                        patched.resolve(demand.getKey().toString()), (List<?>) demand.getValue()
                    )
                );
            }
            final String listed = Files.readString(
                this.temp.resolve("patched.tsv"), StandardCharsets.UTF_8
            );
            final Path sources = this.temp.resolve("sources");
            final String expected = new TreeSet<>(
                demands.keySet().stream().map(Object::toString).map(
                    name -> String.format(
                        "%s\t%s%n", sources.resolve(Paths.get(name).getFileName()), name
                    )
                ).collect(Collectors.toList())
            ).stream().collect(Collectors.joining());
            if (!listed.equals(expected)) {
                failed.add(String.format("patched.tsv: %s, while %s", listed, expected));
            }
            return failed;
        }

        private static Collection<String> missing(final Path file, final List<?> xpaths)
            throws IOException {
            final Collection<String> failed = new ArrayList<>(0);
            if (Files.exists(file)) {
                final XMLDocument xmir = new XMLDocument(file);
                for (final Object xpath : xpaths) {
                    if (xmir.nodes(xpath.toString()).isEmpty()) {
                        failed.add(String.format("%s: %s%nin:%n%s", file, xpath, xmir));
                    }
                }
            } else {
                failed.add(String.format("%s is not patched", file));
            }
            return failed;
        }

        private Path patched() throws IOException {
            Files.write(
                Files.createDirectories(this.temp).resolve("rendered.tsv"),
                ((List<?>) this.story.map().get("rendered")).stream()
                    .map(row -> String.format("%s%n", row))
                    .collect(Collectors.joining())
                    .getBytes(StandardCharsets.UTF_8)
            );
            final Path sources = Files.createDirectories(this.temp.resolve("sources"));
            final Collection<Path> xmirs = new ArrayList<>(0);
            for (final Map.Entry<?, ?> source
                : ((Map<?, ?>) this.story.map().get("eo")).entrySet()) {
                xmirs.add(
                    Files.write(
                        sources.resolve(source.getKey().toString().replace(".eo", ".xmir")),
                        new EoSyntax(source.getValue().toString()).parsed().toString()
                            .getBytes(StandardCharsets.UTF_8)
                    )
                );
            }
            new Patching(
                xmirs, this.temp.resolve("tables"), this.temp.resolve("patched")
            ).exec(this.temp);
            return this.temp.resolve("patched");
        }

        private List<Path> files(final Path patched) throws IOException {
            final List<Path> files;
            if (Files.exists(patched)) {
                try (Stream<Path> all = Files.walk(patched)) {
                    files = all.filter(Files::isRegularFile)
                        .map(patched::relativize)
                        .collect(Collectors.toList());
                }
            } else {
                files = new ListOf<>();
            }
            return files;
        }
    }
}
