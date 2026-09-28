/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.math.BigInteger;
import java.util.Random;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link ChBrief}.
 *
 * @since 0.73.3
 */
final class ChBriefTest {

    @Test
    void shortensTheHashItIsGiven() {
        final long seed = System.nanoTime();
        final String full = String.format("%040x", new BigInteger(160, new Random(seed)));
        MatcherAssert.assertThat(
            String.format("short hash of %s is not its first seven chars, seed %d", full, seed),
            new ChBrief(() -> full).value(),
            Matchers.equalTo(full.substring(0, 7))
        );
    }
}
