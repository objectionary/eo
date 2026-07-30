/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.nio.file.Path;
import org.apache.maven.plugin.testing.stubs.MavenProjectStub;
import org.cactoos.io.ResourceOf;
import org.cactoos.text.TextOf;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test for {@link MjCoverageReport}.
 * @since 0.62.0
 */
@ExtendWith(MktmpResolver.class)
final class MjCoverageReportTest {

    /**
     * Test eo program from resources.
     * @checkstyle ProhibitFieldsInTestClassesCheck (5 lines)
     */
    private String program;

    @BeforeEach
    void setUp() throws Exception {
        this.program = new TextOf(new ResourceOf("org/eolang/maven/mess.eo")).asString();
    }

    @Test
    void buildsAnLcovReportAfterTranspilingAndRunning(@Mktmp final Path temp) throws Exception {
        final Path hits = temp.resolve("hits.txt");
        new FakeMaven(temp)
            .withProgram(this.program)
            .with("coverageFile", hits.toFile())
            .execute(new FakeMaven.Transpile());
        final String[] entry = this.entry(temp.resolve("hits.txt.manifest"));
        new Saved(String.format("%s:%s:1%n", entry[0], entry[1]), hits).value();
        new Moja<>(MjCoverageReport.class)
            .with("coverageFile", hits.toFile())
            .with("foreign", temp.resolve("eo-foreign.csv").toFile())
            .with("foreignFormat", "csv")
            .with("project", new MavenProjectStub())
            .execute();
        MatcherAssert.assertThat(
            "the one locator that was hit must come out as a covered line of the source it belongs to",
            new TextOf(temp.resolve("coverage.info")).asString(),
            Matchers.allOf(
                Matchers.containsString("SF:"),
                Matchers.containsString(String.format("DA:%s,1", entry[1])),
                Matchers.matchesRegex("(?s).*\\bLH:[1-9]\\d*.*"),
                Matchers.containsString("end_of_record")
            )
        );
    }

    @Test
    void failsWhenNoManifestWasEverWritten(@Mktmp final Path temp) {
        final IllegalStateException exception = Assertions.assertThrows(
            IllegalStateException.class,
            () -> new Moja<>(MjCoverageReport.class)
                .with("coverageFile", temp.resolve("never-transpiled.txt").toFile())
                .execute(),
            "coverage-report must fail when the project was never transpiled with a coverage file"
        );
        final StringWriter writer = new StringWriter();
        exception.printStackTrace(new PrintWriter(writer));
        MatcherAssert.assertThat(
            "the failure must explain that no coverage manifest was found, and name the file it looked for",
            writer.toString(),
            Matchers.allOf(
                Matchers.containsString("No coverage manifest found at"),
                Matchers.containsString("never-transpiled.txt.manifest")
            )
        );
    }

    @Test
    void failsWhenNoCoverageFileWasEverConfigured(@Mktmp final Path temp) {
        final IllegalStateException exception = Assertions.assertThrows(
            IllegalStateException.class,
            () -> new Moja<>(MjCoverageReport.class)
                .with("foreign", temp.resolve("eo-foreign.csv").toFile())
                .with("foreignFormat", "csv")
                .execute(),
            "coverage-report must fail when nothing ever asked for instrumentation"
        );
        final StringWriter writer = new StringWriter();
        exception.printStackTrace(new PrintWriter(writer));
        MatcherAssert.assertThat(
            "the failure must name the setting that is missing, rather than let Maven turn the goal away with its own wording",
            writer.toString(),
            Matchers.containsString("eo.coverageFile is not set")
        );
    }

    /**
     * The first entry of a coverage manifest, split into locator and line.
     * @param manifest Path to the manifest file
     * @return The locator and the source line number
     * @throws Exception If fails to read the manifest
     */
    private String[] entry(final Path manifest) throws Exception {
        return new TextOf(manifest).asString()
            .trim().split(System.lineSeparator())[0].split("\t");
    }
}
