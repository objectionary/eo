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
import com.yegor256.WeAreOnline;
import java.io.IOException;
import java.nio.file.Path;
import java.util.Map;
import org.eolang.maven.log.CaptureLogs;
import org.eolang.maven.log.Logs;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link AssembleMojo}.
 *
 * @since 0.1
 * @todo #1602:30min Make up how to get rid of excessive usage of
 *  {@code ParseMojo.DIR}, {@code ResolveMojo.DIR} and so on. It would be nice
 *  to replace them with corresponding classes, or something similar
 * @todo #1602:30min Refactor tests. Logic of AssembleMojo is to run several
 *  phases one-by-one in a loop. Nothing more. Everything else you are trying to
 *  check here is related to particular mojos (and we should check their
 *  behaviour in appropriate tests). In other words there are integration tests
 *  here. And, probably, it is not the best place for them.
 */
@ExtendWith(WeAreOnline.class)
@ExtendWith(MktmpResolver.class)
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
    void assemblesTogether(@Mktmp final Path temp) throws IOException {
        final Map<String, Path> result = new FakeMaven(temp)
            .withHelloWorld()
            .execute(AssembleMojo.class)
            .result();
        final String stdout = "target/%s/org/eolang/io/stdout.%s";
        final String parsed = String.format(stdout, ParseMojo.DIR, AssembleMojo.XMIR);
        final String optimized = String.format(stdout, ShakeMojo.DIR, AssembleMojo.XMIR);
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
    void assemblesNotFailWithFailOnError(@Mktmp final Path temp) throws IOException {
        final Map<String, Path> result = new FakeMaven(temp)
            .withProgram(AssembleMojoTest.INVALID_PROGRAM)
            .execute(new FakeMaven.Optimize())
            .result();
        MatcherAssert.assertThat(
            "Even if the eo program invalid we still have to parse it, but we didn't",
            result.get(String.format("target/%s", ParseMojo.DIR)),
            new ContainsFiles(String.format("**/main.%s", AssembleMojo.XMIR))
        );
        MatcherAssert.assertThat(
            "Even if the eo program invalid we still have to optimize it, but we didn't",
            result.get(String.format("target/%s", ShakeMojo.DIR)),
            new ContainsFiles(String.format("**/main.%s", AssembleMojo.XMIR))
        );
    }

    @CaptureLogs
    @Test
    void assemblesSuccessfullyInOfflineMode(final Logs out, @Mktmp final Path temp) {
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
            Matchers.containsString(
                "No programs were pulled because eo.offline flag is set to TRUE"
            )
        );
    }

    @Test
    void configuresChildParameters(@Mktmp final Path temp) throws IOException {
        final Map<String, Path> res = new FakeMaven(temp)
            .withHelloWorld()
            .with("trackTransformationSteps", true)
            .execute(AssembleMojo.class)
            .result();
        MatcherAssert.assertThat(
            "AssembleMojo should have configured parameters within the Mojos that it uses, but it didn't",
            res,
            Matchers.hasKey(
                String.format("target/%s/foo/x/main.%s", ShakeMojo.DIR, AssembleMojo.XMIR)
            )
        );
    }
}
