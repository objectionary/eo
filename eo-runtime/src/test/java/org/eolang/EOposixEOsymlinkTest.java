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
 * Test case for {@link EOposix$EOsymlink}.
 *
 * @since 0.77.0
 */
final class EOposixEOsymlinkTest {

    @Test
    void refusesTargetWithNul() {
        MatcherAssert.assertThat(
            "the 'target' attribute of posix.symlink carrying a NUL must be refused, but it wasnt",
            Assertions.assertThrows(
                ExAbstract.class,
                () -> new Dataized(
                    new PhApplication(
                        new EOposix$EOsymlink(),
                        "target",
                        new Data.ToPhi(String.join(String.valueOf((char) 0), "плюшка", "щи"))
                    ).take("code")
                ).take(),
                "a 'target' attribute of posix.symlink with a NUL was expected to fail"
            ).getMessage(),
            Matchers.allOf(
                Matchers.containsString("'target' attribute"),
                Matchers.containsString("NUL")
            )
        );
    }

    @Test
    void refusesPathWithNul() {
        MatcherAssert.assertThat(
            "the 'path' attribute of posix.symlink carrying a NUL must be refused, but it wasnt",
            Assertions.assertThrows(
                ExAbstract.class,
                () -> new Dataized(
                    new PhApplication(
                        new PhApplication(
                            new EOposix$EOsymlink(),
                            "target",
                            new Data.ToPhi("плюшка")
                        ),
                        "path",
                        new Data.ToPhi(String.join(String.valueOf((char) 0), "плюшка", "щи"))
                    ).take("code")
                ).take(),
                "a 'path' attribute of posix.symlink with a NUL was expected to fail"
            ).getMessage(),
            Matchers.allOf(
                Matchers.containsString("'path' attribute"),
                Matchers.containsString("NUL")
            )
        );
    }
}
