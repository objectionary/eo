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
 * Test case for {@link EOposix$EOwrite}.
 *
 * @since 0.77.0
 */
final class EOposixEOwriteTest {

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void refusesNegativeFractionalSize() {
        MatcherAssert.assertThat(
            "a size no syscall could mean must be refused, not reported as a write of nothing",
            Assertions.assertThrows(
                ExAbstract.class,
                () -> new Dataized(
                    new PhApplication(
                        new EOposix$EOwrite(),
                        new Bind("descriptor", new Data.ToPhi(0L)),
                        new Bind("buffer", new Data.ToPhi(new byte[]{1, 2})),
                        new Bind("size", new Data.ToPhi(-0.5))
                    )
                ).take(),
                "a negative fractional 'size' attribute was expected to fail"
            ).getMessage(),
            Matchers.containsString("'size' attribute")
        );
    }
}
