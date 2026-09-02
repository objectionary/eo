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
 * @since 0.71.0
 */
@ExtendWith(MktmpResolver.class)
final class MjInferenceReportTest {

    @Test
    void drawsPagesOfProgram(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "a page for the reader must be drawn from the tables, but it wasnt",
            Files.exists(
                new FakeMaven(temp).withProgram(
                    String.join(System.lineSeparator(), "[] > app", "  [] > t", "")
                )
                .execute(MjParse.class)
                .execute(MjInference.class)
                .execute(MjInferenceReport.class)
                .targetPath()
                .getParent()
                .resolve("site")
                .resolve("inference")
                .resolve("index.html")
            ),
            Matchers.is(true)
        );
    }
}
