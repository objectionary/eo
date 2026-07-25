/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collections;
import java.util.stream.Stream;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link Transpiling}.
 * @since 0.61.0
 */
@ExtendWith(MktmpResolver.class)
final class TranspilingTest {

    /**
     * Race threads of one {@link Transpiling} on a single tail path (#5720):
     * the first one transpiles, the rest wait for its lock and read what it
     * cached. With a guard per file each thread locked its own empty map, so
     * every one of them missed and ran the whole XSL train. The train is
     * counted through {@link StMeasured}, which appends one line per shift it
     * executes, so a second pass over {@code to-java.xsl} would show up as a
     * second line.
     * @param temp Temporary directory
     * @throws Exception If fails
     */
    @Test
    @SuppressWarnings("PMD.UnnecessaryLocalRule")
    void transpilesOnlyOnceWhenThreadsRaceOnTheSameTail(@Mktmp final Path temp) throws Exception {
        final TjForeign tojo = new FakeMaven(temp)
            .withProgram("+package foo", "", "# Sample.", "[] > main")
            .execute(MjParse.class)
            .foreignTojos()
            .withXmir()
            .iterator()
            .next();
        final Path measures = temp.resolve("measures.csv");
        final Transpiling transpiling = new Transpiling(
            Collections.singletonList(tojo),
            temp.resolve("transpiled"),
            temp.resolve("generated"),
            temp.resolve("transpile-cache"),
            true,
            "0.0.0",
            false,
            measures,
            new Tracking(false, false),
            false
        );
        new Threaded<>(
            Collections.nCopies(8, tojo),
            each -> transpiling.transpiled(each)
        ).total();
        try (Stream<String> lines = Files.lines(measures)) {
            MatcherAssert.assertThat(
                "Threads racing on one tail path must run the XSL train once, the rest must be cached",
                lines.filter(line -> line.contains("to-java")).count(),
                Matchers.equalTo(1L)
            );
        }
    }
}
