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
 * Test case for {@link Reduction}.
 *
 * @since 0.76.0
 */
@ExtendWith(MktmpResolver.class)
final class ReductionTest {

    @Test
    void reducesBlankNumberToSharedOne(@Mktmp final Path temp) throws IOException {
        final State state = new Settled(ReductionTest.world(temp)).state();
        MatcherAssert.assertThat(
            "the blank number must reduce to the shared one, but it didnt",
            state.kind(
                state.fresh("Φ.number", new Site("Φ.app.x", ""), Values.ROOT)
            ),
            Matchers.hasToString(new Ids(state.shared("Φ.number")).toString())
        );
    }

    @Test
    void reducesDecoratorToWhatItsAtGives(@Mktmp final Path temp) throws IOException {
        final World world = ReductionTest.world(temp);
        final State state = new Settled(world).state();
        MatcherAssert.assertThat(
            "a decorator must reduce to what its @ gives, but it didnt",
            state.kind(
                world.intern(new Value('O', "Φ.five", "Φ.five", Formation.RHO, "", -1, 0))
            ),
            Matchers.hasToString(new Ids(state.shared("Φ.number")).toString())
        );
    }

    @Test
    void keepsMarkOfEmptyVoidsWhenMarkingAgain(@Mktmp final Path temp) throws IOException {
        final World world = ReductionTest.world(temp);
        final State state = new Settled(world).state();
        MatcherAssert.assertThat(
            "a new mark must replace the old one, but it didnt",
            world.value(
                state.mark(state.mark(Values.ROOT, 2), 1)
            ),
            Matchers.equalTo(new Value('A', "", "", 0L, "", Values.ROOT, 1))
        );
    }

    @Test
    void reducesUnknownArgumentToOneCantTell(@Mktmp final Path temp) throws IOException {
        final World world = ReductionTest.world(temp);
        MatcherAssert.assertThat(
            "every unknown going into a void must be one can't tell, but it wasnt",
            new Settled(world).state().reduced(new int[] {Values.GAP, world.unknown("odd")}),
            Matchers.equalTo(new int[] {world.unknown("can't tell")})
        );
    }

    private static World world(final Path temp) throws IOException {
        return new Sources(temp).with("five.eo", "[] > five", "  5 > @").world();
    }
}
