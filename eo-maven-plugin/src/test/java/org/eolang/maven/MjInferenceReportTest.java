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
    void createsNoStageDirectoryWhenInferenceNeverRan(@Mktmp final Path temp) throws IOException {
        new FakeMaven(temp).execute(MjInferenceReport.class);
        MatcherAssert.assertThat(
            "A goal that only reads must leave the target untouched, but it didnt",
            Files.exists(temp.resolve("target")),
            Matchers.is(false)
        );
    }
}
