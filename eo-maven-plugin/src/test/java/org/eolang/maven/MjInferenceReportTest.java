/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

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
 * Test cases for {@link MjInferenceReport}.
 *
 * @since 0.71.0
 */
@ExtendWith(MktmpResolver.class)
final class MjInferenceReportTest {

    @Test
    void drawsAPagePerSourceFromTheAnswers(@Mktmp final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp).withProgram(
            String.join(System.lineSeparator(), "[] > app", "  [] > t", "")
        );
        maven.execute(MjParse.class).execute(MjInference.class);
        Files.writeString(
            maven.targetPath().resolve(maven.dirName("inference")).resolve("answers.xml"),
            "<answers><o loc='Φ.foo.x.main' copies='1'><is loc='Φ.foo.x.main'/></o></answers>"
        );
        maven.execute(MjInferenceReport.class);
        MatcherAssert.assertThat(
            "a page of the program must be drawn from answers.xml, but it wasnt",
            Files.exists(
                maven.targetPath().getParent().resolve("site/inference/foo/x/main.eo.html")
            ),
            Matchers.is(true)
        );
    }

    @Test
    void drawsNothingWithoutAnswers(@Mktmp final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp).withProgram(
            String.join(System.lineSeparator(), "[] > app", "  [] > t", "")
        );
        maven.execute(MjParse.class).execute(MjInference.class);
        Files.delete(
            maven.targetPath().resolve(maven.dirName("inference")).resolve("answers.xml")
        );
        maven.execute(MjInferenceReport.class);
        MatcherAssert.assertThat(
            "no page may be drawn when the engine wrote no answers, but one was",
            Files.exists(maven.targetPath().getParent().resolve("site/inference/index.html")),
            Matchers.is(false)
        );
    }

    @Test
    void createsNoStageDirectoryWhenInferenceNeverRan(@Mktmp final Path temp) throws IOException {
        new FakeMaven(temp).execute(MjInferenceReport.class);
        MatcherAssert.assertThat(
            "A goal that only reads must leave the target untouched, but it didnt",
            Files.exists(temp.resolve("target")),
            Matchers.is(false)
        );
    }
}
