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
import java.util.Collections;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
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
        maven.with("hits", hits.toFile())
            .with("lcov", lcov.toFile())
            .execute(MjCoverageReport.class);
        MatcherAssert.assertThat(
            "the LCOV report must record at least one hit line, but LH:0 everywhere",
            Files.readString(lcov),
            Matchers.not(Matchers.containsString("LH:0"))
        );
    }

    @Test
    void countsEveryRepeatOfTheSameLocation(@Mktmp final Path temp) throws Exception {
        final FakeMaven maven = new FakeMaven(temp).withProgram(
            String.join(System.lineSeparator(), "[] > x", "  42 > y")
        ).execute(MjParse.class);
        final String location = new CoverageManifest().locations(
            new XMLDocument(maven.programTojo().xmir())
        ).iterator().next();
        final int last = location.lastIndexOf(':');
        final String line = location.substring(
            location.lastIndexOf(':', last - 1) + 1, last
        );
        final int repeats = 10_000;
        final Path hits = temp.resolve("coverage.txt");
        Files.write(hits, Collections.nCopies(repeats, location));
        final Path lcov = temp.resolve("coverage.info");
        maven.with("hits", hits.toFile())
            .with("lcov", lcov.toFile())
            .execute(MjCoverageReport.class);
        MatcherAssert.assertThat(
            "every repeat of a location must be counted, however many of them the raw hits file holds, but they werent",
            Files.readString(lcov),
            Matchers.containsString(
                String.format("DA:%s,%d", line, repeats)
            )
        );
    }

    @Test
    void failsTheBuildWhenCoverageIsBelowTheMinimum(@Mktmp final Path temp) throws Exception {
        final FakeMaven maven = new FakeMaven(temp).withProgram(
            String.join(System.lineSeparator(), "[] > x", "  42 > y")
        ).execute(MjParse.class);
        final Path hits = temp.resolve("coverage.txt");
        Files.writeString(hits, "");
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> maven.with("hits", hits.toFile())
                .with("lcov", temp.resolve("coverage.info").toFile())
                .with("minimum", 1.0d)
                .execute(MjCoverageReport.class),
            "coverage-report must fail the build when coverage is below the minimum, but it didnt"
        );
    }

    @Test
    void namesTheSourceThatDoesNotParse(@Mktmp final Path temp) throws Exception {
        final FakeMaven maven = new FakeMaven(temp).withProgram(
            String.join(System.lineSeparator(), "[] > x", "  TRUE > t")
        ).execute(MjParse.class);
        final Path hits = temp.resolve("coverage.txt");
        Files.writeString(hits, "");
        MatcherAssert.assertThat(
            "a source that does not parse must be named by the failure it causes, but it wasnt",
            Assertions.assertThrows(
                IllegalStateException.class,
                () -> maven.with("hits", hits.toFile())
                    .with("lcov", temp.resolve("coverage.info").toFile())
                    .execute(MjCoverageReport.class),
                "coverage-report must fail on a source that does not parse, but it didnt"
            ).getCause().getCause().getMessage(),
            Matchers.containsString("doesn't parse")
        );
    }

    @Test
    void reportsAFileThatHasNothingToInstrument(@Mktmp final Path temp) throws Exception {
        final FakeMaven maven = new FakeMaven(temp).withProgram(
            String.join(
                System.lineSeparator(),
                "[] > foo",
                "  [] > bar /Q.bytes"
            ),
            "foo",
            "foo.eo"
        ).execute(MjParse.class);
        final Path hits = temp.resolve("coverage.txt");
        Files.writeString(hits, "");
        final Path lcov = temp.resolve("coverage.info");
        maven.with("hits", hits.toFile())
            .with("lcov", lcov.toFile())
            .execute(MjCoverageReport.class);
        MatcherAssert.assertThat(
            "a file the manifest names no location in must still get a record of its own, so that the report knows the file exists, but it got none",
            Files.readString(lcov),
            Matchers.stringContainsInOrder("SF:", "foo.eo", "LH:0", "LF:0", "end_of_record")
        );
    }

    @Test
    void doesNotAttributeMergedMembersToThePackageFile(@Mktmp final Path temp) throws Exception {
        final FakeMaven maven = new FakeMaven(temp).withProgram(
            String.join(
                System.lineSeparator(),
                "+architect me@example.com",
                "+version 0.0.0",
                "",
                "[n] > foo",
                "  42 > x",
                "  n > @"
            ),
            "foo",
            "foo.eo"
        ).withProgram(
            String.join(
                System.lineSeparator(),
                "+package foo",
                "",
                "[] > bar",
                "  42 > @"
            ),
            "foo.bar",
            "foo/bar.eo"
        ).execute(MjParse.class).execute(new PpMerge());
        final Path hits = temp.resolve("coverage.txt");
        Files.writeString(hits, "");
        final Path lcov = temp.resolve("coverage.info");
        maven.with("hits", hits.toFile())
            .with("lcov", lcov.toFile())
            .execute(MjCoverageReport.class);
        MatcherAssert.assertThat(
            "the meta directives of the package file must not be reported as DA misses, while its merged member must get a coverage block of its own",
            Files.readString(lcov),
            Matchers.allOf(
                Matchers.matchesPattern(
                    "(?s).*SF:[^\\n]*foo\\.eo\\r?\\n(?:(?!DA:[123],0).)*end_of_record.*"
                ),
                Matchers.containsString("bar.eo")
            )
        );
    }
}
