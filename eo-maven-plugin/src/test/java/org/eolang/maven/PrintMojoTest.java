/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2024 Objectionary.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */
package org.eolang.maven;

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
import org.eolang.maven.util.HmBase;
import org.eolang.maven.util.Home;
import org.eolang.maven.util.Walk;
import org.eolang.parser.EoSyntax;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.yaml.snakeyaml.Yaml;

/**
 * Test cases for {@link PrintMojo}.
 * @since 0.33.0
 */
final class PrintMojoTest {
    @Test
    void printsSuccessfully(@TempDir final Path temp) throws Exception {
        final Home home = new HmBase(temp);
        final Path resources = new File("src/test/resources/org/eolang/maven/print/xmir")
            .toPath();
        final Collection<Path> walk = new Walk(resources);
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
    @ClasspathSource(value = "org/eolang/maven/print/samples/", glob = "**.yaml")
    void printsInStraitNotation(final String pack, @TempDir final Path temp) throws Exception {
        final Map<String, Object> yaml = new Yaml().load(pack);
        MatcherAssert.assertThat(
            "PrintMojo should print EO in strait notation, but it didn't",
            PrintMojoTest.printed(yaml, temp, false).asString(),
            Matchers.equalTo((String) yaml.get("strait"))
        );
    }

    @ParameterizedTest
    @ClasspathSource(value = "org/eolang/maven/print/samples/", glob = "**.yaml")
    void printsInReversedNotation(final String pack, @TempDir final Path temp) throws Exception {
        final Map<String, Object> yaml = new Yaml().load(pack);
        MatcherAssert.assertThat(
            "PrintMojo should print EO in reveresed notation, but it didn't",
            PrintMojoTest.printed(yaml, temp, true).asString(),
            Matchers.equalTo((String) yaml.get("reversed"))
        );
    }

    /**
     * Print XMIR to EO from given pack.
     * @param yaml Yaml pack
     * @param temp Temp directory
     * @param reversed Should notation be reversed or not
     * @return Result printed EO
     * @throws Exception If fails to execute {@link PrintMojo}
     */
    private static Text printed(
        final Map<String, Object> yaml,
        final Path temp,
        final boolean reversed
    ) throws Exception {
        final Home home = new HmBase(temp);
        home.save(
            new EoSyntax(
                "test",
                new InputOf((String) yaml.get("origin"))
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
