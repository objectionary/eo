/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link EOwin32$EOgetenv}.
 *
 * @since 0.77.0
 */
final class EOwin32EOgetenvTest {

    @Test
    void refusesNameWithNul() {
        MatcherAssert.assertThat(
            "the 'name' attribute carrying a NUL must be refused by name, but it wasnt",
            Assertions.assertThrows(
                ExAbstract.class,
                () -> new Dataized(
                    new PhApplication(
                        new EOwin32$EOgetenv(),
                        "name",
                        new Data.ToPhi(String.join(String.valueOf((char) 0), "PATH", "nope"))
                    ).take("code")
                ).take(),
                "a 'name' attribute with a NUL was expected to fail"
            ).getMessage(),
            Matchers.allOf(
                Matchers.containsString("'name' attribute"),
                Matchers.containsString("NUL")
            )
        );
    }
}
