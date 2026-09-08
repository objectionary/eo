/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.github.lombrozo.xnav.Xnav;
import java.io.IOException;
import java.util.Collections;
import java.util.Random;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Tally}.
 *
 * @since 0.76.0
 */
final class TallyTest {

    @Test
    void sumsRewritesOfEveryThread() {
        final long seed = System.nanoTime();
        final int hits = new Random(seed).nextInt(9) + 1;
        final Tally tally = new Tally(doc -> hits, "lowered");
        new Threaded<>(
            Collections.nCopies(16, new Xnav("<o base='ξ.x'/>")), tally::rewrite
        ).total();
        MatcherAssert.assertThat(
            String.format("the tally adds up no rewrite of a thread, seed %d", seed),
            tally.toString(),
            Matchers.equalTo(String.format("%d lowered", hits * 16))
        );
    }

    @Test
    void returnsWhatThePassRewrote() throws IOException {
        final long seed = System.nanoTime();
        final int hits = new Random(seed).nextInt(9) + 1;
        MatcherAssert.assertThat(
            String.format("the tally hides what the pass rewrote, seed %d", seed),
            new Tally(doc -> hits, "folded").rewrite(new Xnav("<o base='ξ.y'/>")),
            Matchers.equalTo(hits)
        );
    }
}
