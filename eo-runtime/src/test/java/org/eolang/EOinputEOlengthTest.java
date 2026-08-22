/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@code input.length}.
 * @since 0.75.0
 */
final class EOinputEOlengthTest {

    @Test
    void measuresAMultiMegabyteInput() {
        final int size = 4 * 1024 * 1024;
        MatcherAssert.assertThat(
            "Length of a multi-megabyte input should be its byte count, but it wasn't",
            new Dataized(
                new PhApplication(
                    Phi.Φ.take("input").copy(),
                    0,
                    new Data.ToPhi(new byte[size]).take("as-input")
                ).take("length")
            ).asNumber().intValue(),
            Matchers.equalTo(size)
        );
    }
}
