/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@code input.length} on a multi-megabyte input.
 * @since 0.75.0
 */
final class InputLengthTest {

    @Test
    void measuresAMultiMegabyteInput() {
        final int size = 2 * 1024 * 1024;
        final Phi block = new Data.ToPhi(new byte[size]).take("as-input");
        final Phi input = new PhApplication(Phi.Φ.take("input").copy(), 0, block);
        MatcherAssert.assertThat(
            "input.length must measure a multi-megabyte input without a stack-depth failure",
            new Dataized(input.take("length")).asNumber().intValue(),
            Matchers.equalTo(size)
        );
    }
}
