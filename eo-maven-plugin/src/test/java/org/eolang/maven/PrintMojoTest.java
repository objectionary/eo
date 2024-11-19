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

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import com.yegor256.farea.Farea;
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
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.yaml.snakeyaml.Yaml;

/**
 * Test cases for {@link PrintMojo}.
 * @since 0.33.0
 */
@ExtendWith(MktmpResolver.class)
final class PrintMojoTest {
    @Test
    void printsSimpleObject(@Mktmp final Path temp) throws Exception {
        new Farea(temp).together(
            f -> {
                f.clean();
                f.files().file("src/main/eo/foo.eo").write(
                    "# Test.\n[] > foo\n".getBytes()
                );
                f.build()
                    .plugins()
                    .appendItself()
                    .execution()
                    .goals("register", "parse");
                f.exec("compile");
                f.files()
                    .file("src/main/xmir/foo.xmir")
                    .save(f.files().file("target/eo/1-parse/foo.xmir").path());
                f.exec("eo:print");
                MatcherAssert.assertThat(
                    "the .phi file is generated",
                    f.files().file("target/generated-sources/eo/foo.eo").exists(),
                    Matchers.is(true)
                );
            }
        );
    }

    @Test
    void printsSuccessfully(@Mktmp final Path temp) throws Exception {
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
    void printsInStraightNotation(final String pack, @Mktmp final Path temp) throws Exception {
        final Map<String, Object> yaml = new Yaml().load(pack);
        MatcherAssert.assertThat(
            "PrintMojo should print EO in straight notation, but it didn't",
            PrintMojoTest.printed(yaml, temp, false).asString(),
            Matchers.equalTo((String) yaml.get("straight"))
        );
    }

    @ParameterizedTest
    @ClasspathSource(value = "org/eolang/maven/print/samples/", glob = "**.yaml")
    void printsInReversedNotation(final String pack, @Mktmp final Path temp) throws Exception {
        final Map<String, Object> yaml = new Yaml().load(pack);
        MatcherAssert.assertThat(
            "PrintMojo should print EO in reversed notation, but it didn't",
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
