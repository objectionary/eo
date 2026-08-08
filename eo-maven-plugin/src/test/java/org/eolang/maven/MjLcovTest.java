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
    void savesRecordedHitAsTracefile(@Mktmp final Path temp) throws Exception {
        final Path lcov = temp.resolve("eo-lcov.info");
        final Path hits = temp.resolve("coverage.txt");
        Files.write(
            hits, String.format("org.eolang.числò:7:2%n").getBytes(StandardCharsets.UTF_8)
        );
        new FakeMaven(temp)
            .with("hits", hits.toFile())
            .with("tracefile", lcov.toFile())
            .execute(MjLcov.class);
        MatcherAssert.assertThat(
            "the hit the tests recorded is not saved as a tracefile record",
            new TextOf(lcov).asString(),
            Matchers.containsString("DA:7,1")
        );
    }

    @Test
    void savesEmptyTracefileWhenNothingWasRecorded(@Mktmp final Path temp) throws Exception {
        final Path lcov = temp.resolve("eo-lcov.info");
        new FakeMaven(temp)
            .with("hits", temp.resolve("absent.txt").toFile())
            .with("tracefile", lcov.toFile())
            .execute(MjLcov.class);
        MatcherAssert.assertThat(
            "a run that recorded nothing is not saved as an empty tracefile",
            new TextOf(lcov).asString(),
            Matchers.equalTo("")
        );
    }
}
