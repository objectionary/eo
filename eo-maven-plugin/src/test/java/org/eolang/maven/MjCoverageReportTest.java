/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.xml.XMLDocument;
import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link MjCoverageReport}.
 * @since 0.75.0
 */
@ExtendWith(MktmpResolver.class)
final class MjCoverageReportTest {

    @Test
    void doesNothingWhenNoCoverageFileIsSet(@Mktmp final Path temp) throws IOException {
        new FakeMaven(temp).withProgram(
            String.join(System.lineSeparator(), "[] > x", "  42 > y")
        ).execute(MjParse.class).execute(MjCoverageReport.class);
        MatcherAssert.assertThat(
            "no LCOV report must be written when eo.coverageFile is unset, but one was",
            Files.exists(temp.resolve("target/coverage.info")),
            Matchers.is(false)
        );
    }

    @Test
    void marksAHitLocationAsCoveredInTheLcovReport(@Mktmp final Path temp) throws Exception {
        final FakeMaven maven = new FakeMaven(temp).withProgram(
            String.join(System.lineSeparator(), "[] > x", "  42 > y")
        ).execute(MjParse.class);
        final String location = new CoverageManifest().locations(
            new XMLDocument(maven.programTojo().xmir())
        ).iterator().next();
        final Path hits = temp.resolve("coverage.txt");
        Files.writeString(hits, String.format("%s%n", location));
        final Path lcov = temp.resolve("coverage.info");
        maven.with("coverageFile", hits.toFile())
            .with("lcovFile", lcov.toFile())
            .execute(MjCoverageReport.class);
        MatcherAssert.assertThat(
            "the LCOV report must record at least one hit line, but LH:0 everywhere",
            Files.readString(lcov),
            Matchers.not(Matchers.containsString("LH:0"))
        );
    }
}
