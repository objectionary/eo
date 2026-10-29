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
        final State state = ReductionTest.settled(temp);
        MatcherAssert.assertThat(
            "the blank number must reduce to the shared one, but it didnt",
            state.reduction().kind(
                state.copies().fresh("Φ.number", new Site("Φ.app.x", ""), Values.ROOT)
            ),
            Matchers.equalTo(new Ids(state.copies().shared("Φ.number")))
        );
    }

    @Test
    void reducesDecoratorToWhatItsAtGives(@Mktmp final Path temp) throws IOException {
        final State state = ReductionTest.settled(temp);
        MatcherAssert.assertThat(
            "a decorator must reduce to what its @ gives, but it didnt",
            state.reduction().kind(
                state.values().intern(new Value('O', "Φ.five", "Φ.five", Formation.RHO, "", -1, 0))
            ),
            Matchers.equalTo(new Ids(state.copies().shared("Φ.number")))
        );
    }

    @Test
    void keepsMarkOfEmptyVoidsWhenMarkingAgain(@Mktmp final Path temp) throws IOException {
        final State state = ReductionTest.settled(temp);
        MatcherAssert.assertThat(
            "a new mark must replace the old one, but it didnt",
            state.values().get(
                state.reduction().mark(state.reduction().mark(Values.ROOT, 2), 1)
            ),
            Matchers.equalTo(new Value('A', "", "", 0L, "", Values.ROOT, 1))
        );
    }

    @Test
    void reducesUnknownArgumentToOneCantTell(@Mktmp final Path temp) throws IOException {
        final State state = ReductionTest.settled(temp);
        MatcherAssert.assertThat(
            "every unknown going into a void must be one can't tell, but it wasnt",
            state.reduction().reduced(new int[] {Values.GAP, state.values().unknown("odd")}),
            Matchers.equalTo(new int[] {state.values().unknown("can't tell")})
        );
    }

    private static State settled(final Path temp) throws IOException {
        return new Sources(temp).with("five.eo", "[] > five", "  5 > @").settled();
    }
}
