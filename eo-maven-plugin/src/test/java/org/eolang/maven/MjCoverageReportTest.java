/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.nio.file.Path;
import org.apache.maven.plugin.MojoFailureException;
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
 * @since 0.58
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
        new Saved(
            String.format("%s:1:1%n", this.locator(temp.resolve("hits.txt.manifest"))),
            hits
        ).value();
        new Moja<>(MjCoverageReport.class)
            .with("coverageFile", hits.toFile())
            .with("foreign", temp.resolve("eo-foreign.csv").toFile())
            .with("foreignFormat", "csv")
            .with("project", new MavenProjectStub())
            .execute();
        MatcherAssert.assertThat(
            "the LCOV tracefile must have one record per instrumented source",
            new TextOf(temp.resolve("coverage.info")).asString(),
            Matchers.allOf(
                Matchers.containsString("SF:"),
                Matchers.containsString("end_of_record")
            )
        );
    }

    @Test
    void failsWhenNoManifestWasEverWritten(@Mktmp final Path temp) {
        MatcherAssert.assertThat(
            "the failure must explain that no coverage manifest was found",
            Assertions.assertThrows(
                IllegalStateException.class,
                () -> new Moja<>(MjCoverageReport.class)
                    .with("coverageFile", temp.resolve("never-transpiled.txt").toFile())
                    .execute(),
                "coverage-report must fail when the project was never transpiled with a coverage file"
            ).getCause(),
            Matchers.instanceOf(MojoFailureException.class)
        );
    }

    /**
     * The locator of the first entry in a coverage manifest.
     * @param manifest Path to the manifest file
     * @return The locator
     * @throws Exception If fails to read the manifest
     */
    private String locator(final Path manifest) throws Exception {
        return new TextOf(manifest).asString()
            .trim().split(System.lineSeparator())[0].split("\t")[0];
    }
}
