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
 * Test case for {@link EOposix$EOstrerror}.
 *
 * @since 0.77.0
 */
final class EOposixEOstrerrorTest {

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void refusesFractionalErrorNumber() {
        MatcherAssert.assertThat(
            "a fractional error number must fail instead of quietly looking up another error",
            Assertions.assertThrows(
                ExAbstract.class,
                () -> new Dataized(
                    new PhApplication(
                        new EOposix$EOstrerror(),
                        new Bind("errno", new Data.ToPhi(2.5))
                    )
                ).take(),
                "a fractional 'errno' attribute was expected to fail"
            ).getMessage(),
            Matchers.allOf(
                Matchers.containsString("'errno' attribute"),
                Matchers.containsString("integer")
            )
        );
    }

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void refusesErrorNumberBeyondIntRange() {
        MatcherAssert.assertThat(
            "an error number past the int range must fail instead of saturating",
            Assertions.assertThrows(
                ExAbstract.class,
                () -> new Dataized(
                    new PhApplication(
                        new EOposix$EOstrerror(),
                        new Bind("errno", new Data.ToPhi(3.0e9))
                    )
                ).take(),
                "an 'errno' attribute past the int range was expected to fail"
            ).getMessage(),
            Matchers.containsString("'errno' attribute")
        );
    }
}
