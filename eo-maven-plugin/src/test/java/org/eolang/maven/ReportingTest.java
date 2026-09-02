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
 * Test case for {@link Reporting}.
 * @since 0.71.0
 */
@ExtendWith(MktmpResolver.class)
final class ReportingTest {

    @Test
    void skipsMissingPreparedXmirs(@Mktmp final Path temp) throws IOException {
        final Path pages = temp.resolve("pages");
        new Reporting(
            temp.resolve("missing"), Files.createDirectories(temp.resolve("tables")), pages
        ).exec();
        MatcherAssert.assertThat(
            "a skipped report must not create any output pages",
            Files.exists(pages),
            Matchers.equalTo(false)
        );
    }
}
