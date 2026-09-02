/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collection;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.cactoos.Text;
import org.cactoos.io.InputOf;
import org.cactoos.map.MapEntry;
import org.cactoos.map.MapOf;
import org.cactoos.text.TextOf;
import org.eolang.jucs.ClasspathSource;
import org.eolang.parser.EoSyntax;
import org.eolang.printer.PenaltyKey;
import org.eolang.xax.XtSticky;
import org.eolang.xax.XtYaml;
import org.eolang.xax.Xtory;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;

/**
 * Test cases for {@link MjPrint}.
 * @since 0.33.0
 */
@ExtendWith(MktmpResolver.class)
final class MjPrintTest {

    /**
     * Temp directory, injected into every test instance.
     */
    @Mktmp
    private Path dir;

    @Test
    void declaresAParameterForEveryPenaltyWeight() {
        final Set<String> declared = new MojoFields().all();
        Assumptions.assumeFalse(declared.isEmpty());
        MatcherAssert.assertThat(
            "a weight a printer pack can vary must be a parameter a build can vary too",
            Stream.of(PenaltyKey.values())
                .map(key -> MjPrintTest.param(key.name()))
                .filter(name -> name.isEmpty() || !declared.contains(name))
                .collect(Collectors.toList()),
            Matchers.empty()
        );
    }

    @Test
    void printsSuccessfully(@Mktmp final Path temp) throws Exception {
        final Path resources = new File(
            "../eo-printer/src/test/resources/org/eolang/printer/print-packs/xmir"
        ).toPath();
        final Collection<Path> walk = new WkDefault(resources);
        Assumptions.assumeTrue(!walk.isEmpty());
        for (final Path source : walk) {
            new Saved(new TextOf(source), temp.resolve(source)).value();
        }
        final Path output = temp.resolve("output");
        new FakeMaven(temp)
            .with("sources", temp.resolve(resources).toFile())
            .with("output", output.toFile())
            .execute(new PpPrint())
            .result();
        for (final Path source : walk) {
            final String src = resources.relativize(source).toString()
                .replace(".xmir", ".eo");
            MatcherAssert.assertThat(
                String.format(
                    "File with name %s should have existed in output directory, but it didn't",
                    src
                ),
                Files.exists(output.resolve(Paths.get(src))),
                Matchers.is(true)
            );
        }
    }

    @Test
    void doesNotMangleADirectoryNameContainingTheXmirSubstring(@Mktmp final Path temp)
        throws Exception {
        final Path source = temp.resolve("xmir/v1.xmir-legacy/main.xmir");
        Files.createDirectories(source.getParent());
        new Saved(
            new EoSyntax(
                new InputOf(
                    String.join(
                        System.lineSeparator(),
                        "+package foo",
                        "",
                        "[] > main"
                    )
                )
            ).parsed().toString(),
            source
        ).value();
        final Path output = temp.resolve("eo");
        new FakeMaven(temp)
            .with("sources", temp.resolve("xmir").toFile())
            .with("output", output.toFile())
            .execute(new PpPrint())
            .result();
        MatcherAssert.assertThat(
            "only the trailing .xmir extension should be replaced, the .xmir substring in the directory name must survive untouched",
            Files.exists(output.resolve("v1.xmir-legacy/main.eo")),
            Matchers.is(true)
        );
    }

    @Test
    void skipsNonXmirFilesInPrintSourcesDir(@Mktmp final Path temp) throws Exception {
        final Path source = temp.resolve("xmir/main.xmir");
        Files.createDirectories(source.getParent());
        new Saved(
            new EoSyntax(
                new InputOf(
                    String.join(
                        System.lineSeparator(),
                        "+package foo",
                        "",
                        "[] > main"
                    )
                )
            ).parsed().toString(),
            source
        ).value();
        new Saved(new InputOf("not xml at all"), temp.resolve("xmir/README.md")).value();
        final Path output = temp.resolve("eo");
        new FakeMaven(temp)
            .with("sources", temp.resolve("xmir").toFile())
            .with("output", output.toFile())
            .execute(new PpPrint())
            .result();
        MatcherAssert.assertThat(
            "the .xmir file should have been printed despite a non-XMIR file sitting next to it",
            Files.exists(output.resolve("main.eo")),
            Matchers.is(true)
        );
    }

    @ParameterizedTest
    @ClasspathSource(value = "org/eolang/maven/print-packs", glob = "**.yaml")
    void printsXmirToEo(final String pack) throws Exception {
        final Xtory xtory = new XtSticky(new XtYaml(pack));
        Assumptions.assumeTrue(xtory.map().get("skip") == null);
        MatcherAssert.assertThat(
            "PrintMojo should print EO in straight notation, but it didn't",
            MjPrintTest.printed(xtory, this.dir).asString(),
            Matchers.equalTo(this.expected(xtory))
        );
    }

    private String expected(final Xtory xtory) {
        final String origin = (String) xtory.map().get("origin");
        final String expected;
        if (xtory.map().containsKey("printed")) {
            expected = (String) xtory.map().get("printed");
            MatcherAssert.assertThat(
                "The 'printed' section repeats 'origin' verbatim and must be deleted from the pack, since a pack without 'printed' already expects the printer to reproduce its 'origin'",
                expected,
                Matchers.not(Matchers.equalTo(origin))
            );
        } else {
            expected = origin;
        }
        return expected;
    }

    private static Text printed(final Xtory xtory, final Path temp)
        throws Exception {
        new Saved(
            new EoSyntax(
                new InputOf(xtory.map().get("origin").toString())
            ).parsed().toString(),
            temp.resolve("xmir/foo/x/main.xmir")
        ).value();
        final FakeMaven maven = new FakeMaven(temp)
            .with("sources", temp.resolve("xmir").toFile())
            .with("output", temp.resolve("eo").toFile());
        final Object pins = xtory.map().get("penalties");
        if (pins != null) {
            for (final Map.Entry<?, ?> pin : ((Map<?, ?>) pins).entrySet()) {
                final String param = MjPrintTest.param(pin.getKey().toString());
                if (!param.isEmpty()) {
                    maven.with(param, ((Number) pin.getValue()).intValue());
                }
            }
        }
        return new TextOf(
            maven.execute(MjPrint.class).result().get("eo/foo/x/main.eo")
        );
    }

    private static String param(final String key) {
        return new MapOf<>(
            new MapEntry<>("INDENT", "indent"),
            new MapEntry<>("BRACKET", "bracket"),
            new MapEntry<>("LEADING", "leading"),
            new MapEntry<>("PHI", "phi"),
            new MapEntry<>("IF", "conditional"),
            new MapEntry<>("EXCESS", "excess"),
            new MapEntry<>("SYMBOL", "symbol"),
            new MapEntry<>("SPACE", "space"),
            new MapEntry<>("WIDTH", "width"),
            new MapEntry<>("STEP", "step")
        ).getOrDefault(key, "");
    }
}
