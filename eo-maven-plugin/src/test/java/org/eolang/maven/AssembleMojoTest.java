/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2023 Objectionary.com
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

import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Map;
import org.cactoos.set.SetOf;
import org.eolang.maven.objectionary.Objectionary;
import org.eolang.maven.util.Home;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.io.TempDir;

/**
 * Test case for {@link AssembleMojo}.
 *
 * @since 0.1
 * @todo #2120:90min Use FakeMaven in AssembleMojoTest.
 *  It's better to refactor of the tests inside {@link AssembleMojoTest}.
 *  Refactoring will simplify the existing code and reduce the total lines number.
 *  When the refactoring is done, remove this puzzle.
 */
@ExtendWith(OnlineCondition.class)
final class AssembleMojoTest {

    @Test
    void assemblesTogether(@TempDir final Path temp) throws IOException {
        final Map<String, Path> result = new FakeMaven(temp)
            .withProgram(
                "+alias stdout org.eolang.io.stdout",
                "",
                "[x] > main",
                "  (stdout \"Hello!\" x).print"
            )
            .execute(AssembleMojo.class)
            .result();
        final String parsed = String.format("target/%s/org/eolang/io/stdout.%s",
            ParseMojo.DIR,
            TranspileMojo.EXT
        );
        final String optimized = String.format("target/%s/org/eolang/io/stdout.%s",
            OptimizeMojo.DIR,
            TranspileMojo.EXT
        );
        final String pulled = String.format("target/%s/org/eolang/io/stdout.eo",
            PullMojo.DIR
        );
        MatcherAssert.assertThat(
            String.format(
                "AssembleMojo should have parsed stdout object %s, but wasn't",
                parsed
            ),
            result.containsKey(parsed),
            Matchers.is(true)
        );
        MatcherAssert.assertThat(
            String.format(
                "AssembleMojo should have optimized stdout object %s, but wasn't ",
                optimized
            ),
            result.containsKey(optimized),
            Matchers.is(true)
        );
        MatcherAssert.assertThat(
            String.format(
                "AssembleMojo should have pulled stdout object %s, but wasn't ",
                pulled
            ),
            result.containsKey(pulled),
            Matchers.is(true)
        );
        MatcherAssert.assertThat(
            "AssembleMojo should have placed runtime library under classes directory, but wasn't ",
            result.get("target/classes").toAbsolutePath(),
            new ContainsFile("**/eo-runtime-*.jar")
        );
    }

    @Test
    void assemblesNotFailWithFailOnErrorFlag2(@TempDir final Path temp) throws Exception {
        final Map<String, Path> result = new FakeMaven(temp)
            .withProgram(
                "+alias stdout org.eolang.io.stdout",
                "+home https://github.com/objectionary/eo",
                "+package test",
                "+version 0.0.0",
                "",
                "[x] < wrong>",
                "  (stdout \"Hello!\" x).print"
            )
            .with("failOnError", false)
            .execute(AssembleMojo.class).result();
        MatcherAssert.assertThat(
            "Even if the eo program invalid we still have parse it, but we didn't",
            result.get(String.format("target/%s", ParseMojo.DIR)),
            new ContainsFile(String.format("**/main.%s", TranspileMojo.EXT))
        );
        MatcherAssert.assertThat(
            "Since the eo program invalid we shouldn't have optimized it, but we did",
            result.get(String.format("target/%s", OptimizeMojo.DIR)),
            Matchers.not(new ContainsFile(String.format("**/main.%s", TranspileMojo.EXT)))
        );
    }

    @Test
    void assemblesNotFailWithFailOnErrorFlag(@TempDir final Path temp) throws Exception {
        final Path src = temp.resolve("src");
        new Home(src).save(
            String.join(
                "\n",
                "+alias stdout org.eolang.io.stdout",
                "+home https://github.com/objectionary/eo",
                "+package test",
                "+version 0.0.0",
                "",
                "[x] < wrong>\n  (stdout \"Hello!\" x).print\n"
            ),
            Paths.get("wrong.eo")
        );
        new Home(src).save(
            String.join(
                "\n",
                "+alias stdout org.eolang.io.stdout",
                "+home https://github.com/objectionary/eo",
                "+package test",
                "+version 0.0.0",
                "",
                "[x] > main\n  (stdout \"Hello!\" x).print\n"
            ),
            Paths.get("main.eo")
        );
        final Path target = temp.resolve("target");
        new Moja<>(RegisterMojo.class)
            .with("foreign", temp.resolve("eo-foreign.json").toFile())
            .with("foreignFormat", "json")
            .with("sourcesDir", src.toFile())
            .with("includeSources", new SetOf<>("**.eo"))
            .execute();
        new Moja<>(AssembleMojo.class)
            .with("outputDir", temp.resolve("out").toFile())
            .with("targetDir", target.toFile())
            .with("foreign", temp.resolve("eo-foreign.json").toFile())
            .with("foreignFormat", "json")
            .with("placed", temp.resolve("list").toFile())
            .with("cache", temp.resolve("cache/parsed"))
            .with("skipZeroVersions", true)
            .with("failOnError", false)
            .with("central", Central.EMPTY)
            .with("ignoreTransitive", true)
            .with("plugin", FakeMaven.pluginDescriptor())
            .with(
                "objectionary",
                new Objectionary.Fake()
            )
            .execute();
        MatcherAssert.assertThat(
            new Home(target).exists(
                Paths.get(
                    String.format(
                        "%s/org/eolang/io/stdout.%s",
                        ParseMojo.DIR, TranspileMojo.EXT
                    )
                )
            ),
            Matchers.is(true)
        );
        MatcherAssert.assertThat(
            new Home(target).exists(
                Paths.get(
                    String.format(
                        "%s/main.%s",
                        ParseMojo.DIR, TranspileMojo.EXT
                    )
                )
            ),
            Matchers.is(true)
        );
    }

}
