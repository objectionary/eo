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
 * Test case for {@link Callbacks}.
 *
 * @since 0.76.0
 */
@ExtendWith(MktmpResolver.class)
final class CallbacksTest {

    @Test
    void fillsEmptyVoidOfFormationCalledBackWithWhatJavaMakes(@Mktmp final Path temp)
        throws IOException {
        final State state = new Sources(
            temp,
            "memory.eo",
            "[] > memory\n  [] > use /Q.number\n    ? > size /Q.number\n    ? > scope /{Q.block}\n",
            "block.eo", "[] > block\n  42 > size\n",
            "app.eo", "[] > app\n  memory.use > @\n    5\n    [b]\n      b.size > @\n"
        ).settled();
        final int copy = state.values.intern(
            new Value('O', "Φ.app.φ.α1", "Φ.app.φ", 1L | Formation.RHO, "", -1, 0)
        );
        MatcherAssert.assertThat(
            "the formation called back must get a copy with Java's block in its void, but it didnt",
            state.values.full(state.rows.row(copy, "b").get(0)),
            Matchers.equalTo("Φ.block@java{ρ}")
        );
    }

    @Test
    void notesCopyWhoseRhoOnlyJavaFills(@Mktmp final Path temp) throws IOException {
        final State state = new Sources(
            temp,
            "jobs.eo", "[] > jobs\n  [] > run /Q.number\n    ? > job\n",
            "app.eo", "[] > app\n  jobs.run > @\n    [^]\n      42 > @\n"
        ).settled();
        MatcherAssert.assertThat(
            "a copy handed to an atom with a declared ^ nobody binds must be noted, but it wasnt",
            state.callbacks.handedToJava(
                state.values.intern(
                    new Value('O', "Φ.app.φ.α0", "Φ.app.φ.α0", 0L, "", -1, 0)
                )
            ),
            Matchers.is(true)
        );
    }
}
