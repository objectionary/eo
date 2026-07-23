/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.nio.file.Path;
import java.util.Map;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test for {@link CoverageReport}.
 * @since 0.58
 */
@ExtendWith(MktmpResolver.class)
final class CoverageReportTest {

    /**
     * Name of the raw hits file, reused across test fixtures.
     * @checkstyle ProhibitFieldsInTestClassesCheck (5 lines)
     */
    private final String hitsfile = "hits.txt";

    /**
     * Name of the manifest file, reused across test fixtures.
     * @checkstyle ProhibitFieldsInTestClassesCheck (5 lines)
     */
    private final String manifestfile = "hits.txt.manifest";

    @Test
    void marksManifestLinesAsCoveredOnlyWhenTheirLocatorWasHit(
        @Mktmp final Path temp
    ) throws IOException {
        final Path manifest = temp.resolve(this.manifestfile);
        final Path hits = temp.resolve(this.hitsfile);
        new Saved(
            String.join(
                System.lineSeparator(),
                "Φ.main\t5",
                "Φ.main.α0\t6",
                "Φ.other\t2",
                ""
            ),
            manifest
        ).value();
        new Saved(
            String.format("Φ.main:5:1%n"),
            hits
        ).value();
        MatcherAssert.assertThat(
            "the touched line must be marked as covered and the untouched ones as not",
            new CoverageReport(
                manifest, hits, new ObjectSources(Map.of("main", "main.eo", "other", "other.eo"))
            ).text(),
            Matchers.allOf(
                Matchers.containsString("SF:main.eo"),
                Matchers.containsString("DA:5,1"),
                Matchers.containsString("DA:6,0"),
                Matchers.containsString("SF:other.eo"),
                Matchers.containsString("DA:2,0"),
                Matchers.containsString(String.format("LF:2%n")),
                Matchers.containsString(String.format("LH:1%n"))
            )
        );
    }

    @Test
    void computesThePercentageOfCoveredLines(@Mktmp final Path temp) throws IOException {
        final Path manifest = temp.resolve(this.manifestfile);
        final Path hits = temp.resolve(this.hitsfile);
        new Saved(
            String.join(
                System.lineSeparator(),
                "Φ.main\t5",
                "Φ.main.α0\t6",
                "Φ.main.α1\t7",
                "Φ.main.α2\t8",
                ""
            ),
            manifest
        ).value();
        new Saved(
            String.join(
                System.lineSeparator(),
                "Φ.main:5:1",
                "Φ.main.α0:6:3",
                ""
            ),
            hits
        ).value();
        MatcherAssert.assertThat(
            "two out of four instrumented lines were hit, so coverage must be 50%",
            new CoverageReport(manifest, hits, new ObjectSources(Map.of("main", "main.eo")))
                .percentage(),
            Matchers.closeTo(50.0d, 0.001d)
        );
    }

    @Test
    void treatsAMissingHitsFileAsZeroPercentCovered(@Mktmp final Path temp) throws IOException {
        final Path manifest = temp.resolve(this.manifestfile);
        new Saved("Φ.main\t5", manifest).value();
        MatcherAssert.assertThat(
            "a manifest with no matching hits file means nothing was ever touched",
            new CoverageReport(
                manifest, temp.resolve(this.hitsfile), new ObjectSources(Map.of("main", "main.eo"))
            ).percentage(),
            Matchers.closeTo(0.0d, 0.001d)
        );
    }

    @Test
    void reportsFullCoverageWhenNothingIsInstrumented(@Mktmp final Path temp) throws IOException {
        final Path manifest = temp.resolve(this.manifestfile);
        new Saved("", manifest).value();
        MatcherAssert.assertThat(
            "an empty manifest has nothing to miss, so it must count as fully covered",
            new CoverageReport(
                manifest, temp.resolve(this.hitsfile), new ObjectSources(Map.of())
            ).percentage(),
            Matchers.closeTo(100.0d, 0.001d)
        );
    }

    @Test
    void mergesMultipleLocatorsOnTheSameLineIntoOneDaEntry(
        @Mktmp final Path temp
    ) throws IOException {
        final Path manifest = temp.resolve(this.manifestfile);
        final Path hits = temp.resolve(this.hitsfile);
        new Saved(
            String.join(
                System.lineSeparator(),
                "Φ.main.a\t9",
                "Φ.main.b\t9",
                ""
            ),
            manifest
        ).value();
        new Saved(String.format("Φ.main.b:9:2%n"), hits).value();
        MatcherAssert.assertThat(
            "a line with two locators must appear once, covered, if either was hit",
            new CoverageReport(manifest, hits, new ObjectSources(Map.of("main", "main.eo")))
                .text(),
            Matchers.allOf(
                Matchers.containsString("DA:9,1"),
                Matchers.not(Matchers.containsString("DA:9,0"))
            )
        );
    }
}
