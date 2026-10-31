/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.nio.file.Path;
import java.util.Set;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link Rules}.
 *
 * @since 0.76.0
 */
@ExtendWith(MktmpResolver.class)
final class RulesTest {

    @Test
    void keepsWhatWaitsForStop(@Mktmp final Path temp) throws IOException {
        final Rules rules = new Rules(new State(new Sources(temp).world()));
        rules.wish(new Wish('3', "Φ.oven.φ", 8, 1));
        MatcherAssert.assertThat(
            "what waits for a stop must be pending, but it wasnt",
            rules.pending(),
            Matchers.contains(new Wish('3', "Φ.oven.φ", 8, 1))
        );
    }

    @Test
    void leavesDecidedWishOutOfPending(@Mktmp final Path temp) throws IOException {
        final Rules rules = new Rules(new State(new Sources(temp).world()));
        rules.wish(new Wish('e', "", -1, 44));
        rules.take(Set.of(new Wish('e', "", -1, 44)));
        MatcherAssert.assertThat(
            "what a stop decided must not be pending any more, but it was",
            rules.pending(),
            Matchers.empty()
        );
    }

    @Test
    void forgetsWhatAnswerThatLostItFoundWaiting(@Mktmp final Path temp) throws IOException {
        final State state = new State(new Sources(temp).world());
        state.ask(
            new Key('k', 5, "", "", "", 0),
            () -> {
                state.wish(new Wish('b', "Φ.oven.φ", 3, 5));
                return new Ints();
            }
        );
        state.stale(new Key('k', 5, "", "", "", 0));
        state.invalidate();
        MatcherAssert.assertThat(
            "what an answer that lost it found waiting must be forgotten, but it wasnt",
            state.wishes(),
            Matchers.empty()
        );
    }
}
