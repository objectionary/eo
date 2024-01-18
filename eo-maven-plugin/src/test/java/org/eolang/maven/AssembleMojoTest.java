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

import com.yegor256.WeAreOnline;
import java.io.IOException;
import java.nio.file.Path;
import java.util.Map;
import org.cactoos.map.MapEntry;
import org.eolang.maven.hash.ChCached;
import org.eolang.maven.hash.ChRemote;
import org.eolang.maven.hash.CommitHash;
import org.eolang.maven.hash.CommitHashesMap;
import org.eolang.maven.log.CaptureLogs;
import org.eolang.maven.log.Logs;
import org.eolang.maven.objectionary.ObjsDefault;
import org.eolang.maven.objectionary.OyRemote;
import org.eolang.maven.util.JoinedUnderscore;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.io.TempDir;

/**
 * Test case for {@link AssembleMojo}.
 *
 * @todo #1602:30min Make up how to get rid of excessive usage of
 *  {@code ParseMojo.DIR}, {@code ResolveMojo.DIR} and so on. It would be nice
 *  to replace them with corresponding classes, or something similar
 * @todo #1602:30min Refactor tests. Logic of AssembleMojo is to run several
 *  phases one-by-one in a loop. Nothing more. Everything else you are trying to
 *  check here is related to particular mojos (and we should check their
 *  behaviour in appropriate tests). In other words there are integration tests
 *  here. And, probably, it is not the best place for them.
 * @todo #2612:30min Enable the test {@link AssembleMojoTest#assemblesTogetherWithVersions(Path)}.
 *  The test was disabled because varargs were removed in EO 0.34.0. So objects that are downloaded
 *  from older repositories are not parsed successfully because of the presence of varargs there.
 *  So we need to make 2-3 releases and then refactor the test with more fresh versions. Don't
 *  forget to remove the puzzle.
 * @since 0.1
 */
@ExtendWith(WeAreOnline.class)
final class AssembleMojoTest {

    /**
     * Invalid eo program for testing.
     */
    static final String[] INVALID_PROGRAM = {
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
            new ContainsFiles("**/eo-runtime-*.jar")
        );
    }

    @Test
    @Disabled
    @ExtendWith(WeAreOnline.class)
    void assemblesTogetherWithVersions(@TempDir final Path temp) throws Exception {
        final Map<String, CommitHash> hashes = new CommitHashesMap.Fake();
        final CommitHash master = new ChCached(new ChRemote("master"));
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
            .with("hash", master)
            .execute(AssembleMojo.class)
            .result();
        final String stdout = "**/io/stdout";
        final String fifth = new JoinedUnderscore(stdout, "17f8929.xmir").asString();
        final String sixth = new JoinedUnderscore(stdout, "9c93528.xmir").asString();
        final String path = "target/%s/org/eolang";
        final String parse = String.format(path, ParseMojo.DIR);
        final String optimize = String.format(path, OptimizeMojo.DIR);
        final String pull = String.format(path, PullMojo.DIR);
        final String resolve = String.format("target/%s", ResolveMojo.DIR);
        final String seq = "**/seq.xmir";
        final String string = "**/string.xmir";
        final String hash = String.join(".", master.value(), "eo");
        final String[] jars = {
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
                new JoinedUnderscore(stdout, "17f8929.eo").asString(),
                new JoinedUnderscore(stdout, "9c93528.eo").asString(),
                new JoinedUnderscore("**/seq", hash).asString(),
                new JoinedUnderscore("**/string", hash).asString()
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
    void assemblesNotFailWithFailOnError(@TempDir final Path temp) throws IOException {
        final Map<String, Path> result = new FakeMaven(temp)
            .withProgram(AssembleMojoTest.INVALID_PROGRAM)
            .execute(new FakeMaven.Optimize())
            .result();
        MatcherAssert.assertThat(
            "Even if the eo program invalid we still have to parse it, but we didn't",
            result.get(String.format("target/%s", ParseMojo.DIR)),
            new ContainsFiles(String.format("**/main.%s", TranspileMojo.EXT))
        );
        MatcherAssert.assertThat(
            "Even if the eo program invalid we still have to optimize it, but we didn't",
            result.get(String.format("target/%s", OptimizeMojo.DIR)),
            new ContainsFiles(String.format("**/main.%s", TranspileMojo.EXT))
        );
    }

    @CaptureLogs
    @Test
    void assemblesSuccessfullyInOfflineMode(final Logs out, @TempDir final Path temp) {
        Assertions.assertDoesNotThrow(
            () -> new FakeMaven(temp)
                .withHelloWorld()
                .with("offline", true)
                .execute(AssembleMojo.class),
            "AssembleMojo should have executed successfully with eo.offline=TRUE, but it didn't"
        );
        MatcherAssert.assertThat(
            "While execution AssembleMojo log should have contained message about offline mode, but it didn't",
            String.join("\n", out.captured()),
            Matchers.containsString("No programs were pulled because eo.offline flag is TRUE")
        );
    }

    @Test
    void configuresChildParameters(@TempDir final Path temp) throws IOException {
        final Map<String, Path> res = new FakeMaven(temp)
            .withHelloWorld()
            .with("trackOptimizationSteps", true)
            .execute(AssembleMojo.class)
            .result();
        MatcherAssert.assertThat(
            "AssembleMojo should have configured parameters within the Mojos that it uses, but it didn't",
            res,
            Matchers.hasKey(
                String.format("target/%s/foo/x/main/01-not-empty-atoms.xml", OptimizeMojo.STEPS)
            )
        );
        MatcherAssert.assertThat(
            "AssembleMojo should have configured parameters within the Mojos that it uses, but it didn't",
            res,
            Matchers.hasKey(
                String.format("target/%s/foo/x/main.%s", OptimizeMojo.DIR, TranspileMojo.EXT)
            )
        );
    }
}
