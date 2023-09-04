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
import java.util.Map;
import org.cactoos.map.MapEntry;
import org.eolang.maven.hash.CommitHash;
import org.eolang.maven.hash.CommitHashesMap;
import org.eolang.maven.objectionary.ObjsDefault;
import org.eolang.maven.objectionary.OyRemote;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.io.TempDir;

/**
 * Test case for {@link AssembleMojo}.
 *
 * @since 0.1
 * @todo #1602:30min Create new object that will join two strings with "_".
 *  {@link Place} object makes a path for versioned objects using "_" as
 *  delimiter between name and hash. Here to test stored files after
 *  {@link AssembleMojo} execution "joinedWithUnderscore" function was
 *  introduced. So there's a code duplication and an ugly design. Need to
 *  create a new object that will join two strings with underscore and use it
 *  here and in {@link Place}.
 * @todo #1602:30min Make up how to get rid of excessive usage of
 *  {@code ParseMojo.DIR}, {@code ResolveMojo.DIR} and so on. It would be nice
 *  to replace them with corresponding classes, or something similar
 * @todo #1602:30min Refactor tests. Logic of AssembleMojo is to run several
 *  phases one-by-one in a loop. Nothing more. Everything else you are trying to
 *  check here is related to particular mojos (and we should check their
 *  behaviour in appropriate tests). In other words there are integration tests
 *  here. And, probably, it is not the best place for them.
 */
@ExtendWith(OnlineCondition.class)
final class AssembleMojoTest {

    /**
     * Invalid eo program for testing.
     */
    private static final String[] INVALID_PROGRAM = {
        "+alias stdout org.eolang.io.stdout",
        "+home https://github.com/objectionary/eo",
        "+package test",
        "+version 0.0.0",
        "",
        "[x] < wrong>",
        "  (stdout \"Hello!\" x).print",
    };

    @Test
    void assemblesTogether(@TempDir final Path temp) throws IOException {
        final Map<String, Path> result = new FakeMaven(temp)
            .withHelloWorld()
            .execute(AssembleMojo.class)
            .result();
        final String stdout = "target/%s/org/eolang/io/stdout.%s";
        final String parsed = String.format(stdout, ParseMojo.DIR, TranspileMojo.EXT);
        final String optimized = String.format(stdout, OptimizeMojo.DIR, TranspileMojo.EXT);
        final String pulled = String.format(stdout, PullMojo.DIR, "eo");
        MatcherAssert.assertThat(
            String.format(
                "AssembleMojo should have parsed stdout object %s, but didn't",
                parsed
            ),
            result.containsKey(parsed),
            Matchers.is(true)
        );
        MatcherAssert.assertThat(
            String.format(
                "AssembleMojo should have optimized stdout object %s, but didn't",
                optimized
            ),
            result.containsKey(optimized),
            Matchers.is(true)
        );
        MatcherAssert.assertThat(
            String.format(
                "AssembleMojo should have pulled stdout object %s, but didn't",
                pulled
            ),
            result.containsKey(pulled),
            Matchers.is(true)
        );
        MatcherAssert.assertThat(
            "AssembleMojo should have placed runtime library under classes directory, but didn't",
            result.get("target/classes").toAbsolutePath(),
            new ContainsFile("**/eo-runtime-*.jar")
        );
    }

    @Test
    void assemblesTogetherWithVersions(@TempDir final Path temp) throws Exception {
        final Map<String, CommitHash> hashes = new CommitHashesMap.Fake();
        final CommitHash master = hashes.get("master");
        final CommitHash five = hashes.get("0.28.5");
        final CommitHash six = hashes.get("0.28.6");
        final Map<String, Path> result = new FakeMaven(temp)
            .withProgram(
                "+package org.eolang\n",
                "[x] > main",
                "  seq > @",
                "    QQ.io.stdout|0.28.5",
                "      \"Hello five\"",
                "    QQ.io.stdout|0.28.6",
                "      \"Hello six\""
            )
            .with("withVersions", true)
            .with(
                "objectionaries",
                new ObjsDefault(
                    new MapEntry<>(master, new OyRemote(master)),
                    new MapEntry<>(five, new OyRemote(five)),
                    new MapEntry<>(six, new OyRemote(six))
                )
            )
            .execute(AssembleMojo.class)
            .result();
        final String stdout = "**/io/stdout";
        final String fifth = AssembleMojoTest.joinedWithUnderscore(stdout, "17f8929.xmir");
        final String sixth = AssembleMojoTest.joinedWithUnderscore(stdout, "9c93528.xmir");
        final String path = "target/%s/org/eolang";
        final String parse = String.format(path, ParseMojo.DIR);
        final String optimize = String.format(path, OptimizeMojo.DIR);
        final String pull = String.format(path, PullMojo.DIR);
        final String resolve = String.format("target/%s", ResolveMojo.DIR);
        final String seq = "**/seq.xmir";
        final String string = "**/string.xmir";
        final String hash = String.join(".", master.value(), "eo");
        final String[] jars = new String[] {
            "**/eo-runtime-0.28.5.jar",
            "**/eo-runtime-0.28.6.jar",
        };
        MatcherAssert.assertThat(
            String.format(
                "AssembleMojo should placed parsed files under %s directory, but didn't",
                parse
            ),
            result.get(parse).toAbsolutePath(),
            new ContainsFiles(fifth, sixth, seq, string)
        );
        MatcherAssert.assertThat(
            String.format(
                "AssembleMojo should placed optimized files under %s directory, but didn't",
                optimize
            ),
            result.get(optimize).toAbsolutePath(),
            new ContainsFiles(fifth, sixth, seq, string)
        );
        MatcherAssert.assertThat(
            String.format(
                "AssembleMojo should placed pulled files under %s directory, but didn't",
                pull
            ),
            result.get(pull).toAbsolutePath(),
            new ContainsFiles(
                AssembleMojoTest.joinedWithUnderscore(stdout, "17f8929.eo"),
                AssembleMojoTest.joinedWithUnderscore(stdout, "9c93528.eo"),
                AssembleMojoTest.joinedWithUnderscore("**/seq", hash),
                AssembleMojoTest.joinedWithUnderscore("**/string", hash)
            )
        );
        MatcherAssert.assertThat(
            String.format(
                "AssembleMojo should placed pulled files under %s directory, but didn't",
                resolve
            ),
            result.get(resolve).toAbsolutePath(),
            new ContainsFiles(jars)
        );
        MatcherAssert.assertThat(
            "AssembleMojo should have placed runtime libraries under classes directory, but didn't",
            result.get("target/classes").toAbsolutePath(),
            new ContainsFiles(jars)
        );
    }

    @Test
    void assemblesNotFailWithFailOnErrorFlag(@TempDir final Path temp) throws IOException {
        final Map<String, Path> result = new FakeMaven(temp)
            .withProgram(AssembleMojoTest.INVALID_PROGRAM)
            .with("failOnError", false)
            .execute(AssembleMojo.class).result();
        MatcherAssert.assertThat(
            "Even if the eo program invalid we still have to parse it, but we didn't",
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
    void doesNotAssembleIfFailOnErrorFlagIsTrue(@TempDir final Path temp) {
        final Class<IllegalStateException> expected = IllegalStateException.class;
        Assertions.assertThrows(
            expected,
            () -> new FakeMaven(temp)
                .withProgram(AssembleMojoTest.INVALID_PROGRAM)
                .execute(AssembleMojo.class),
            String.format("AssembleMojo should have failed with %s, but didn't", expected)
        );
    }

    private static String joinedWithUnderscore(final String first, final String second) {
        return String.join("_", first, second);
    }
}
