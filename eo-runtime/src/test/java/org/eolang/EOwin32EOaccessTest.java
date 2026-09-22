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
 * Test case for {@link EOwin32$EOaccess}.
 *
 * @since 0.77.0
 */
final class EOwin32EOaccessTest {

    @Test
    void refusesPathWithNul() {
        MatcherAssert.assertThat(
            "the 'path' attribute carrying a NUL must be refused by name, but it wasnt",
            Assertions.assertThrows(
                ExAbstract.class,
                () -> new Dataized(
                    new PhApplication(
                        new EOwin32$EOaccess(),
                        new Bind("mode", new Data.ToPhi(0L)),
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

    @Test
    void refusesFractionalMode() {
        MatcherAssert.assertThat(
            "the 'mode' attribute must be refused by name when it is not an integer, but it wasnt",
            Assertions.assertThrows(
                ExAbstract.class,
                () -> new Dataized(
                    new PhApplication(
                        new EOwin32$EOaccess(),
                        new Bind("mode", new Data.ToPhi(3.9)),
                        new Bind("path", new Data.ToPhi("one"))
                    )
                ).take(),
                "a fractional 'mode' attribute was expected to fail"
            ).getMessage(),
            Matchers.allOf(
                Matchers.containsString("'mode' attribute"),
                Matchers.containsString("integer")
            )
        );
    }
}
