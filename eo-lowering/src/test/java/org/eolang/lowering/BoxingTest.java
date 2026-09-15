/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.jcabi.xml.XML;
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
import org.hamcrest.io.FileMatchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;

/**
 * Test case for {@link Boxing}.
 *
 * <p>What a box is, and which formation deserves one, is said by the packs
 * in {@code box-packs}: each one carries the program a human would write,
 * the tables {@code eo:inference} would have left about it, and the XPaths
 * the boxed XMIR must satisfy. What is left here are the mechanics no EO
 * source can express: how the numbers of two files follow one another,
 * where a copy lands, and what the stage refuses to do at all.</p>
 *
 * @since 0.74.0
 */
@ExtendWith(MktmpResolver.class)
final class BoxingTest {

    /**
     * Temp directory, injected into every test instance, since a parameterized
     * test cannot also take one as an argument.
     */
    @Mktmp
    private Path dir;

    @ParameterizedTest
    @ClasspathSource(value = "org/eolang/lowering/box-packs/", glob = "**.yaml")
    void boxesTheProgramOfAPack(final String yaml) throws IOException {
        MatcherAssert.assertThat(
            "every XPath of the pack must match what the boxing wrote, but some didnt",
            BoxingTest.unmatched(new XtSticky(new XtYaml(yaml)), this.dir),
            Matchers.empty()
        );
    }

    @Test
    void countsTheBoxesOfOneFileOnIntoTheNext(@Mktmp final Path temp) throws IOException {
        final Path sources = BoxingTest.sources(temp);
        MatcherAssert.assertThat(
            "the first box of the second file must carry on from the last of the first, but it doesnt",
            new XMLDocument(
                BoxingTest.boxed(
                    new ListOf<>(
                        BoxingTest.parsed(
                            sources.resolve("a.xmir"),
                            BoxingTest.program(
                                "[a] > one",
                                "  a.plus 1 > @",
                                "  [b] > inner",
                                "    b.plus 2 > @"
                            )
                        ),
                        BoxingTest.parsed(
                            sources.resolve("b.xmir"),
                            BoxingTest.program("[c] > two", "  c.plus 3 > @")
                        )
                    ),
                    temp
                ).resolve("two.xmir")
            ).xpath("/object/o[@name='two']/o[@name='λ']/text()"),
            Matchers.contains("L_box_3_object")
        );
    }

    @Test
    void namesTheCopyAfterTheLocatorOfItsObject(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "the copy must be named after the object it holds and not after the file it came from, but it isnt",
            BoxingTest.boxed(
                Collections.singletonList(
                    BoxingTest.parsed(
                        BoxingTest.sources(temp).resolve("whatever.xmir"),
                        BoxingTest.program(
                            "+package demo", "", "[a b] > gap", "  a.plus b > @"
                        )
                    )
                ),
                temp
            ).resolve("demo.gap.xmir").toFile(),
            FileMatchers.anExistingFile()
        );
    }

    @Test
    void refusesTwoSourcesThatWouldBeBoxedIntoOneFile(@Mktmp final Path temp) throws IOException {
        final Path sources = BoxingTest.sources(temp);
        final Collection<Path> both = new ListOf<>(
            BoxingTest.parsed(
                sources.resolve("a.xmir"),
                BoxingTest.program("[a] > gap", "  a.plus 1 > @")
            ),
            BoxingTest.parsed(
                sources.resolve("b.xmir"),
                BoxingTest.program("[b] > gap", "  b.plus 2 > @")
            )
        );
        MatcherAssert.assertThat(
            "the failure must name the copy two objects are fighting over, but it doesnt",
            Assertions.assertThrows(
                IllegalStateException.class,
                () -> BoxingTest.boxed(both, temp),
                "two objects of one name must fail the boxing"
            ).getMessage(),
            Matchers.containsString("gap.xmir")
        );
    }

    @Test
    void failsNamingTheTablesItCannotFind(@Mktmp final Path temp) {
        final Path tables = temp.resolve("tables");
        MatcherAssert.assertThat(
            "the failure must name the directory the tables are missing from, but it doesnt",
            Assertions.assertThrows(
                IllegalStateException.class,
                () -> new Boxing(new ListOf<>(), tables, temp.resolve("lower")).exec(),
                "tables that are not there must fail the boxing"
            ).getMessage(),
            Matchers.containsString(tables.toString())
        );
    }

    @Test
    void makesTheBoxedDirectoryWithNothingToBox(@Mktmp final Path temp) throws IOException {
        try (Stream<Path> made = Files.list(BoxingTest.boxed(new ListOf<>(), temp))) {
            MatcherAssert.assertThat(
                "a build with no sources must still be given the directory, empty, but it wasnt",
                made.collect(Collectors.toList()),
                Matchers.empty()
            );
        }
    }

    private static Path boxed(final Collection<Path> sources, final Path temp)
        throws IOException {
        return BoxingTest.boxed(sources, BoxingTest.tables(temp, "<provides/>"), temp);
    }

    private static Path boxed(
        final Collection<Path> sources, final Path tables, final Path temp
    ) throws IOException {
        final Path home = temp.resolve("lower");
        new Boxing(sources, tables, home).exec();
        return home.resolve("boxed");
    }

    private static Path tables(final Path temp, final String provides) throws IOException {
        final Path made = Files.createDirectories(temp.resolve("tables"));
        Files.write(
            made.resolve("provides.xml"), provides.getBytes(StandardCharsets.UTF_8)
        );
        return made;
    }

    private static Path sources(final Path temp) throws IOException {
        return Files.createDirectories(temp.resolve("sources"));
    }

    private static Path parsed(final Path file, final String program) throws IOException {
        return Files.write(
            file,
            new EoSyntax(program).parsed().toString().getBytes(StandardCharsets.UTF_8)
        );
    }

    private static String program(final String... lines) {
        return String.join(System.lineSeparator(), lines)
            .concat(System.lineSeparator());
    }

    private static Collection<String> unmatched(final Xtory pack, final Path temp)
        throws IOException {
        final Collection<String> failed = new ArrayList<>(0);
        for (final Object key : pack.map().keySet()) {
            if (!Arrays.asList("eo", "provides", "xmir").contains(key)) {
                failed.add(String.format("unknown key: %s", key));
            }
        }
        final Path sources = BoxingTest.sources(temp);
        final Collection<Path> paths = new ArrayList<>(0);
        for (final Map.Entry<?, ?> source
            : ((Map<?, ?>) pack.map().get("eo")).entrySet()) {
            paths.add(
                BoxingTest.parsed(
                    sources.resolve(source.getKey().toString().replace(".eo", ".xmir")),
                    source.getValue().toString()
                )
            );
        }
        failed.addAll(
            BoxingTest.absent(
                pack,
                BoxingTest.boxed(
                    paths,
                    BoxingTest.tables(
                        temp,
                        pack.map().getOrDefault("provides", "<provides/>").toString()
                    ),
                    temp
                )
            )
        );
        return failed;
    }

    private static Collection<String> absent(final Xtory pack, final Path base)
        throws IOException {
        final Collection<String> failed = new ArrayList<>(0);
        for (final Map.Entry<?, ?> entry
            : ((Map<?, ?>) pack.map().get("xmir")).entrySet()) {
            final String name = entry.getKey().toString();
            final Path file = base.resolve(name);
            if (Files.exists(file)) {
                final XML document = new XMLDocument(file);
                for (final Object xpath : (List<?>) entry.getValue()) {
                    if (document.nodes(xpath.toString()).isEmpty()) {
                        failed.add(String.format("%s: %s", name, xpath));
                    }
                }
            } else {
                failed.add(String.format("no boxed XMIR for %s", name));
            }
        }
        return failed;
    }
}
