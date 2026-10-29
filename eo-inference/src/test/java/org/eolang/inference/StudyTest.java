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
 * Test case for {@link Study}.
 *
 * @since 0.76.0
 */
@ExtendWith(MktmpResolver.class)
final class StudyTest {

    @Test
    void keepsCopyAsWrittenOfFormationNobodyCalls(@Mktmp final Path temp) throws IOException {
        final State state = new Sources(temp, "kite.eo", "[wind] > kite\n  wind > @\n").settled();
        MatcherAssert.assertThat(
            "the copy as written of a formation nobody calls must be kept, but it wasnt",
            state.study.kept(
                state.values.intern(new Value('O', "Φ.kite", "Φ.kite", Formation.RHO, "", -1, 0))
            ),
            Matchers.is(true)
        );
    }

    @Test
    void doesNotKeepCopyAsWrittenOfFormationSomeoneCalls(@Mktmp final Path temp)
        throws IOException {
        final State state = new Sources(
            temp,
            "kite.eo", "[wind] > kite\n  wind > @\n",
            "beach.eo", "[] > beach\n  kite 5 > @\n"
        ).settled();
        MatcherAssert.assertThat(
            "the copy as written of a formation someone calls must not be kept, but it was",
            state.study.kept(
                state.values.intern(new Value('O', "Φ.kite", "Φ.kite", Formation.RHO, "", -1, 0))
            ),
            Matchers.is(false)
        );
    }

    @Test
    void givesStudyCopyInsideRealCopyAround(@Mktmp final Path temp) throws IOException {
        final State state = new Sources(
            temp,
            "kite.eo", "[wind] > kite\n  [knot] > tail\n    knot > @\n  wind > @\n",
            "beach.eo", "[] > beach\n  kite 5 > @\n"
        ).settled();
        final int host = state.values.intern(
            new Value('O', "Φ.kite", "Φ.beach.φ", 1L | Formation.RHO, "", -1, 0)
        );
        MatcherAssert.assertThat(
            "a formation nobody calls must get a study copy inside the real copy around it, but it didnt",
            state.study.kept(
                state.values.intern(
                    new Value(
                        'O', "Φ.kite.tail", String.format("in:%s", state.values.full(host)),
                        Formation.RHO, "", -1, 0
                    )
                )
            ),
            Matchers.is(true)
        );
    }
}
