/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import java.security.SecureRandom;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.DisabledOnOs;
import org.junit.jupiter.api.condition.OS;

/**
 * Test case for {@link EOposix$EOgetenv}.
 *
 * @since 0.77.0
 */
final class EOposixEOgetenvTest {

    @Test
    void refusesNameWithNul() {
        MatcherAssert.assertThat(
            "the 'name' attribute carrying a NUL must be refused by name, but it wasnt",
            Assertions.assertThrows(
                ExAbstract.class,
                () -> new Dataized(
                    new PhApplication(
                        new EOposix$EOgetenv(),
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

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void putsOneIntoTheCodeOfVariableItFinds() {
        MatcherAssert.assertThat(
            "a variable that is set must put the number one into the code, but it didnt",
            new Dataized(
                new PhApplication(
                    new EOposix$EOgetenv(), "name", new Data.ToPhi("PATH")
                ).take("v🌵cde")
            ).asNumber(),
            Matchers.equalTo(1.0)
        );
    }

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void putsZeroIntoTheCodeOfVariableItMisses() {
        MatcherAssert.assertThat(
            "a variable that is not set must put the number zero into the code, but it didnt",
            new Dataized(
                new PhApplication(
                    new EOposix$EOgetenv(),
                    "name",
                    new Data.ToPhi(
                        String.format("EO_UNSET_%d", new SecureRandom().nextInt(1_000_000))
                    )
                ).take("v🌵cde")
            ).asNumber(),
            Matchers.equalTo(0.0)
        );
    }
}
