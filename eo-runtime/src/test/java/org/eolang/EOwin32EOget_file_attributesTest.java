/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.DisabledOnOs;
import org.junit.jupiter.api.condition.OS;

/**
 * Test case for {@link EOwin32$EOget_file_attributes}.
 *
 * @since 0.77.0
 */
final class EOwin32EOget_file_attributesTest {

    @Test
    @DisabledOnOs({OS.LINUX, OS.MAC})
    void refusesPathWithNul() {
        MatcherAssert.assertThat(
            "a path carrying a NUL must be refused by name, since a C string ends there",
            Assertions.assertThrows(
                ExAbstract.class,
                () -> new Dataized(
                    new PhApplication(
                        new EOwin32$EOget_file_attributes(),
                        new Bind(
                            "path",
                            new Data.ToPhi(String.join(String.valueOf((char) 0), "one", "two"))
                        )
                    )
                ).take(),
                "a 'path' attribute with a NUL was expected to fail"
            ).getMessage(),
            Matchers.allOf(
                Matchers.containsString("'path' attribute"),
                Matchers.containsString("NUL")
            )
        );
    }
}
