/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
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
import org.cactoos.Text;
import org.cactoos.io.InputOf;
import org.cactoos.text.TextOf;
import org.eolang.jucs.ClasspathSource;
import org.eolang.parser.EoSyntax;
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
 * Test cases for {@link PrintMojo}.
 * @since 0.33.0
 */
@ExtendWith(MktmpResolver.class)
final class PrintMojoTest {

    @Test
    void printsSuccessfully(@Mktmp final Path temp) throws Exception {
        final HmBase home = new HmBase(temp);
        final Path resources = new File(
            "../eo-parser/src/test/resources/org/eolang/parser/print-packs/xmir"
        ).toPath();
        final Collection<Path> walk = new Walk(resources);
        Assumptions.assumeTrue(!walk.isEmpty());
        for (final Path source : walk) {
            home.save(new TextOf(source), source);
        }
        final Path output = temp.resolve("output");
        final Path sources = temp.resolve(resources);
        new FakeMaven(temp)
            .with("printSourcesDir", sources.toFile())
            .with("printOutputDir", output.toFile())
            .execute(new FakeMaven.Print())
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

    @ParameterizedTest
    @ClasspathSource(value = "org/eolang/maven/print-packs", glob = "**.yaml")
    void printsXmirToEo(final String pack, @Mktmp final Path temp) throws Exception {
        final Xtory xtory = new XtSticky(new XtYaml(pack));
        Assumptions.assumeTrue(xtory.map().get("skip") == null);
        MatcherAssert.assertThat(
            "PrintMojo should print EO in straight notation, but it didn't",
            PrintMojoTest.printed(xtory, temp, false).asString(),
            Matchers.equalTo((String) xtory.map().get("printed"))
        );
    }

    /**
     * Print XMIR to EO from given pack.
     * @param xtory XaX story
     * @param temp Temp directory
     * @param reversed Should notation be reversed or not
     * @return Result printed EO
     * @throws Exception If fails to execute {@link PrintMojo}
     */
    private static Text printed(final Xtory xtory, final Path temp, final boolean reversed)
        throws Exception {
        final HmBase home = new HmBase(temp);
        home.save(
            new EoSyntax(
                "test",
                new InputOf(xtory.map().get("origin").toString())
            ).parsed().toString(),
            Paths.get("xmir/foo/x/main.xmir")
        );
        final Map<String, Path> result = new FakeMaven(temp)
            .with("printSourcesDir", temp.resolve("xmir").toFile())
            .with("printOutputDir", temp.resolve("eo").toFile())
            .with("printReversed", reversed)
            .execute(PrintMojo.class)
            .result();
        return new TextOf(result.get("eo/foo/x/main.eo"));
    }
}
