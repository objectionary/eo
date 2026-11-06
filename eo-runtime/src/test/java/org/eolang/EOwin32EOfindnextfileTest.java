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
 * Test case for {@link EOwin32$EOfind_next_file}.
 *
 * @since 0.77.0
 */
final class EOwin32EOfindnextfileTest {

    @Test
    @EnabledOnOs(OS.WINDOWS)
    void leavesNameEmptyAtTheEndOfSearch(@TempDir final Path temp) throws IOException {
        Files.write(temp.resolve("плюшка"), new byte[0]);
        MatcherAssert.assertThat(
            "the name of a search that ran out must be an empty string, but it isnt",
            new Dataized(
                new PhApplication(
                    new EOwin32$EOfind_next_file(),
                    "search",
                    new Data.ToPhi(
                        new Dataized(
                            new PhApplication(
                                new EOwin32$EOfind_first_file(),
                                "pattern",
                                new Data.ToPhi(temp.resolve("плюшка").toString())
                            ).take("code")
                        ).asNumber().intValue()
                    )
                ).take("name")
            ).asString(),
            Matchers.emptyString()
        );
    }
}
