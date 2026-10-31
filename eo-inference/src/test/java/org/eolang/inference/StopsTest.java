/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.nio.file.Path;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link Stops}.
 *
 * @since 0.76.0
 */
@ExtendWith(MktmpResolver.class)
final class StopsTest {

    @Test
    void decidesNothingOnceSettled(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "a stop after the run settled must decide nothing, but it decided something",
            new Settled(
                new Sources(temp).with("kite.eo", "[wind] > kite", "  wind > @").world()
            ).state().decide(),
            Matchers.is(false)
        );
    }

    @Test
    void decidesWhatWaitsBeforeStudyingAnything(@Mktmp final Path temp) throws IOException {
        final State state = new State(
            new Sources(temp).with("kite.eo", "[wind] > kite", "  wind > @").world()
        );
        state.seed();
        state.wish(new Wish('3', "Φ.kite.φ", 9, 0));
        state.decide();
        MatcherAssert.assertThat(
            "a stop with a rule waiting must decide that rule, but it didnt",
            state.taken(new Wish('3', "Φ.kite.φ", 9, 0)),
            Matchers.is(true)
        );
    }
}
