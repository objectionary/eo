/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import java.nio.file.Path;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.EnabledOnOs;
import org.junit.jupiter.api.condition.OS;
import org.junit.jupiter.api.io.TempDir;

/**
 * Test case for {@link EOwin32$EOget_file_attributes}.
 *
 * @since 0.77.0
 */
final class EOwin32EOgetfileattributesTest {

    @Test
    void refusesPathWithNul() {
        MatcherAssert.assertThat(
            "the 'path' attribute carrying a NUL must be refused by name, but it wasnt",
            Assertions.assertThrows(
                ExAbstract.class,
                () -> new Dataized(
                    new PhApplication(
                        new EOwin32$EOget_file_attributes(),
                        "path",
                        new Data.ToPhi(String.join(String.valueOf((char) 0), "плюшка", "щи"))
                    ).take("code")
                ).take(),
                "a 'path' attribute with a NUL was expected to fail"
            ).getMessage(),
            Matchers.allOf(
                Matchers.containsString("'path' attribute"),
                Matchers.containsString("NUL")
            )
        );
    }

    @Test
    @EnabledOnOs(OS.WINDOWS)
    void reportsAttributesWhenPathExists(@TempDir final Path temp) {
        MatcherAssert.assertThat(
            "existing path must not return INVALID_FILE_ATTRIBUTES (-1)",
            new Dataized(
                new PhApplication(
                    new EOwin32$EOget_file_attributes(),
                    "path",
                    new Data.ToPhi(temp.toString())
                ).take("code")
            ).asNumber().intValue(),
            Matchers.not(Matchers.equalTo(-1))
        );
    }

    @Test
    @EnabledOnOs(OS.WINDOWS)
    void leavesMessageEmptyWhenPathExists(@TempDir final Path temp) {
        MatcherAssert.assertThat(
            "the message for an existing path must be an empty string, but it wasnt",
            new Dataized(
                new PhApplication(
                    new EOwin32$EOget_file_attributes(),
                    "path",
                    new Data.ToPhi(temp.toString())
                ).take("message")
            ).asString(),
            Matchers.emptyString()
        );
    }

    @Test
    @EnabledOnOs(OS.WINDOWS)
    void reportsInvalidCodeWhenPathIsAbsent(@TempDir final Path temp) {
        MatcherAssert.assertThat(
            "absent path must return INVALID_FILE_ATTRIBUTES (-1)",
            new Dataized(
                new PhApplication(
                    new EOwin32$EOget_file_attributes(),
                    "path",
                    new Data.ToPhi(temp.resolve("absent").toString())
                ).take("code")
            ).asNumber().intValue(),
            Matchers.equalTo(-1)
        );
    }

    @Test
    @EnabledOnOs(OS.WINDOWS)
    void reportsErrorWhenPathIsAbsent(@TempDir final Path temp) {
        MatcherAssert.assertThat(
            "the message for an absent path must report the Win32 error code",
            new Dataized(
                new PhApplication(
                    new EOwin32$EOget_file_attributes(),
                    "path",
                    new Data.ToPhi(temp.resolve("absent").toString())
                ).take("message")
            ).asString(),
            Matchers.startsWith("Win32 error ")
        );
    }
}
