/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import org.cactoos.text.TextOf;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link MjLcov}.
 * @since 0.74.0
 */
@ExtendWith(MktmpResolver.class)
final class MjLcovTest {

    @Test
    void savesTouchedObjectsAsTracefile(@Mktmp final Path temp) throws Exception {
        final Path hits = temp.resolve("coverage.txt");
        Files.write(
            hits,
            String.format("числò.plus:7:2%nчислò.plus:7:9%ntorn line%n")
                .getBytes(StandardCharsets.UTF_8)
        );
        final Path tracefile = temp.resolve("eo-lcov.info");
        new FakeMaven(temp)
            .with("hits", hits.toFile())
            .with("tracefile", tracefile.toFile())
            .execute(MjLcov.class);
        MatcherAssert.assertThat(
            "the two objects the tests touched on one line are not counted on it",
            new TextOf(tracefile).asString(),
            Matchers.stringContainsInOrder("SF:", "числò/plus.eo", "DA:7,2", "LF:1", "LH:1")
        );
    }

    @Test
    void savesTracefileWhereTheParameterNamesIt(@Mktmp final Path temp) throws Exception {
        final Path tracefile = temp.resolve("отчёт").resolve("объекты.info");
        new FakeMaven(temp)
            .with("hits", temp.resolve("coverage.txt").toFile())
            .with("tracefile", tracefile.toFile())
            .execute(MjLcov.class);
        MatcherAssert.assertThat(
            "the tracefile is not at the path the parameter names",
            Files.exists(tracefile),
            Matchers.is(true)
        );
    }
}
