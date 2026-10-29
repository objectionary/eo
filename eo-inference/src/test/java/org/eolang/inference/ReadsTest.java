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
 * Test case for {@link Reads}.
 *
 * @since 0.76.0
 */
@ExtendWith(MktmpResolver.class)
final class ReadsTest {

    @Test
    void failsReadOffDatum(@Mktmp final Path temp) throws IOException {
        final State state = ReadsTest.settled(temp);
        MatcherAssert.assertThat(
            "a read off a datum must fail and say what it reads, but it didnt",
            state.values().full(
                state.reads().attr(Values.DATA, "size", new Site("Φ.app.φ", "")).get(0)
            ),
            Matchers.equalTo("X:data.size")
        );
    }

    @Test
    void readsTerminationOffTermination(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "a read off T must be T, but it wasnt",
            ReadsTest.settled(temp).reads().attr(Values.TERM, "size", new Site("Φ.app.φ", "")),
            Matchers.equalTo(new Ids(Values.TERM))
        );
    }

    @Test
    void readsUnboundVoidAsTermination(@Mktmp final Path temp) throws IOException {
        final State state = ReadsTest.settled(temp);
        MatcherAssert.assertThat(
            "a void the copy leaves unbound must read T, but it didnt",
            state.reads().attr(
                state.values().intern(
                    new Value('O', "Φ.vase", "Φ.app.φ.ρ", 1L | Formation.RHO, "", -1, 0)
                ),
                "water", new Site("Φ.app.φ", "")
            ),
            Matchers.equalTo(new Ids(Values.TERM))
        );
    }

    @Test
    void failsReadOfNameCompleteFormationLacks(@Mktmp final Path temp) throws IOException {
        final State state = ReadsTest.settled(temp);
        MatcherAssert.assertThat(
            "a read of a name a complete formation lacks must fail, but it didnt",
            state.values().full(
                state.reads().attr(
                    state.values().intern(
                        new Value('O', "Φ.cat", "Φ.cat", Formation.RHO, "", -1, 0)
                    ),
                    "bark", new Site("Φ.app.x", "")
                ).get(0)
            ),
            Matchers.equalTo("X:Φ.cat.bark")
        );
    }

    private static State settled(final Path temp) throws IOException {
        return new Sources(temp)
            .with("vase.eo", "[flower water] > vase", "  flower > @", "  water > wet")
            .with("app.eo", "[] > app", "  (vase 5).wet > @")
            .with("cat.eo", "[] > cat", "  \"meow\" > voice")
            .settled();
    }
}
