/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.nio.file.Path;
import java.util.Map;
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
    void namesReadWithNoAnswerAsLoop(@Mktmp final Path temp) throws IOException {
        final World world = new Sources(temp)
            .with("loop.eo", "[^] > loop", "  ^.loop.value > value", "  value > @")
            .world();
        final State state = new Settled(world).state();
        state.step();
        MatcherAssert.assertThat(
            "a read with no answer must be told as a loop by both copies that say it, but it wasnt",
            new Telling(state, world).told().sets(),
            Matchers.hasEntry(
                Matchers.equalTo("Φ.loop.value"),
                Matchers.equalTo(Map.of("?unknown-loop", 2))
            )
        );
    }

    @Test
    void leavesUnknownOnlyJavaFillsInAnswers(@Mktmp final Path temp) throws IOException {
        final World world = EndStepTest.world(temp);
        final State state = new Settled(world).state();
        state.step();
        MatcherAssert.assertThat(
            "an answer with the unknown only Java fills must keep it after the end step, but it didnt",
            state.stored(EndStepTest.read(world)),
            Matchers.hasToString(new Ids(Values.JGAP).toString())
        );
    }

    private static Key read(final World world) {
        return new Key(
            'e',
            world.intern(new Value('O', "Φ.app", "Φ.app", 0L, "", -1, 0)),
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
