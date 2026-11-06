/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.EnabledOnOs;
import org.junit.jupiter.api.condition.OS;
import org.junit.jupiter.api.io.TempDir;

/**
 * Test case for {@link EOwin32$EOfind_first_file}.
 *
 * @since 0.77.0
 */
final class EOwin32EOfindfirstfileTest {

    @Test
    @EnabledOnOs(OS.WINDOWS)
    void leavesNameEmptyWhenSearchFails(@TempDir final Path temp) {
        MatcherAssert.assertThat(
            "the name of a search that failed must be an empty string, but it isnt",
            new Dataized(
                new PhApplication(
                    new EOwin32$EOfind_first_file(),
                    "pattern",
                    new Data.ToPhi(temp.resolve("плюшка").resolve("*").toString())
                ).take("name")
            ).asString(),
            Matchers.emptyString()
        );
    }

    @Test
    @EnabledOnOs(OS.WINDOWS)
    void leavesMessageEmptyWhenSearchWorks(@TempDir final Path temp) throws IOException {
        Files.write(temp.resolve("щи"), new byte[0]);
        MatcherAssert.assertThat(
            "the message of a search that worked must be an empty string, but it isnt",
            new Dataized(
                new PhApplication(
                    new EOwin32$EOfind_first_file(),
                    "pattern",
                    new Data.ToPhi(temp.resolve("*").toString())
                ).take("message")
            ).asString(),
            Matchers.emptyString()
        );
    }
}
