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
 * Test case for {@link EndStep}.
 *
 * @since 0.76.0
 */
@ExtendWith(MktmpResolver.class)
final class EndStepTest {

    @Test
    void marksReadWithNoAnswer(@Mktmp final Path temp) throws IOException {
        final World world = new Sources(temp)
            .with("loop.eo", "[] > loop", "  ^.loop.value > value", "  value > @")
            .world();
        final State state = new Settled(world).state();
        state.step();
        MatcherAssert.assertThat(
            "a read with no answer must be marked at the end, but it wasnt",
            state.token(
                "Φ.loop.value",
                world.intern(new Value('O', "Φ.loop", "Φ.loop", Formation.RHO, "", -1, 0))
            ),
            Matchers.equalTo(EndStep.CANT)
        );
    }

    @Test
    void makesDecidedOpenVoidPlainInAnswers(@Mktmp final Path temp) throws IOException {
        final World world = EndStepTest.world(temp);
        final State state = new Settled(world).state();
        state.step();
        MatcherAssert.assertThat(
            "an answer with a decided open void must hold a plain one after the end step, but it didnt",
            state.stored(EndStepTest.read(world)),
            Matchers.equalTo(new Ids(Values.GAP))
        );
    }

    @Test
    void keepsAsideAnswerWithDecidedOpenVoid(@Mktmp final Path temp) throws IOException {
        final World world = EndStepTest.world(temp);
        final State state = new Settled(world).state();
        state.step();
        MatcherAssert.assertThat(
            "the answer as it was before the end step must be kept aside, but it wasnt",
            state.untidy(
                EndStepTest.read(world), state.stored(EndStepTest.read(world))
            ).array(),
            Matchers.equalTo(new int[] {Values.JGAP})
        );
    }

    private static Key read(final World world) {
        return new Key(
            'e',
            world.intern(new Value('O', "Φ.app", "Φ.app", Formation.RHO, "", -1, 0)),
            "", "Φ.app.φ", "", 0
        );
    }

    private static World world(final Path temp) throws IOException {
        return new Sources(temp).with(
            "os.eo",
            "[] > os",
            "  [] > check /Q.os.status",
            "  [code] > status",
            "    code > @"
        ).with(
            "app.eo",
            "[] > app",
            "  os.check.code > @"
        ).world();
    }
}
