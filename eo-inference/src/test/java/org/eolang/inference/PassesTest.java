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
 * Test case for {@link Passes}.
 *
 * @since 0.76.0
 */
@ExtendWith(MktmpResolver.class)
final class PassesTest {

    @Test
    void walksFormationAsWrittenInsideCopiesAround(@Mktmp final Path temp) throws IOException {
        final State state = new Sources(
            temp, "house.eo", "[] > house\n  [] > room\n    42 > @\n"
        ).settled();
        MatcherAssert.assertThat(
            "a nested formation must be walked as written, inside the copy of the one around, but it wasnt",
            state.passes.known().get(
                state.values.intern(
                    new Value('O', "Φ.house.room", "Φ.house.room", Formation.RHO, "", -1, 0)
                )
            ),
            Matchers.is(true)
        );
    }

    @Test
    void remembersValueOnce(@Mktmp final Path temp) throws IOException {
        final State state = new State(new Sources(temp).program(), new Names());
        state.passes.remember(Values.TERM);
        state.passes.remember(Values.TERM);
        MatcherAssert.assertThat(
            "a value remembered twice must be among the copies once, but it wasnt",
            state.passes.contexts().size(),
            Matchers.equalTo(1)
        );
    }

    @Test
    void stopsUsingCopiesOnceSettled(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "the passes must be over once settled, but they werent",
            new Sources(temp, "house.eo", "[] > house\n  42 > @\n").settled().passes.frozen(),
            Matchers.is(true)
        );
    }
}
