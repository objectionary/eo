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
 * Test case for {@link EOposix$EOrename}.
 *
 * @since 0.77.0
 */
final class EOposixEOrenameTest {

    @Test
    void refusesSourceWithNul() {
        MatcherAssert.assertThat(
            "the 'from' attribute carrying a NUL must be refused by name, but it wasnt",
            Assertions.assertThrows(
                ExAbstract.class,
                () -> new Dataized(
                    new PhApplication(
                        new EOposix$EOrename(),
                        "from",
                        new Data.ToPhi(String.join(String.valueOf((char) 0), "плюшка", "щи"))
                    ).take("code")
                ).take(),
                "a 'from' attribute with a NUL was expected to fail"
            ).getMessage(),
            Matchers.allOf(
                Matchers.containsString("'from' attribute"),
                Matchers.containsString("NUL")
            )
        );
    }

    @Test
    void refusesTargetWithNul() {
        MatcherAssert.assertThat(
            "the 'to' attribute carrying a NUL must be refused by name, but it wasnt",
            Assertions.assertThrows(
                ExAbstract.class,
                () -> new Dataized(
                    new PhApplication(
                        new PhApplication(
                            new EOposix$EOrename(),
                            "from",
                            new Data.ToPhi("плюшка")
                        ),
                        "to",
                        new Data.ToPhi(String.join(String.valueOf((char) 0), "плюшка", "щи"))
                    ).take("code")
                ).take(),
                "a 'to' attribute with a NUL was expected to fail"
            ).getMessage(),
            Matchers.allOf(
                Matchers.containsString("'to' attribute"),
                Matchers.containsString("NUL")
            )
        );
    }
}
