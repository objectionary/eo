/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.io.IOException;
import java.util.Random;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test for {@link Trees}.
 * @since 0.74
 */
final class TreesTest {

    @Test
    void walksSameTextOnlyOnce() throws IOException {
        final long seed = System.nanoTime();
        final String text = String.format(
            "# Комментарий %d.%n[] > obj%d%n  42 > @%n", seed, new Random(seed).nextInt(1000)
        );
        final Trees trees = new Trees.TsShared();
        MatcherAssert.assertThat(
            String.format("the same text was walked more than once, seed %d", seed),
            trees.of(text),
            Matchers.sameInstance(trees.of(text))
        );
    }
}
