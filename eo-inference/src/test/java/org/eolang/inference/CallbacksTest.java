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
        final World world = new Sources(temp).with(
            "memory.eo",
            "[] > memory",
            "  [] > use /Q.number",
            "    ? > size /Q.number",
            "    ? > scope /{Q.block}"
        ).with(
            "block.eo",
            "[] > block",
            "  42 > size"
        ).with(
            "app.eo",
            "[] > app",
            "  memory.use > @",
            "    5",
            "    [b]",
            "      b.size > @"
        ).world();
        MatcherAssert.assertThat(
            "the formation called back must get a copy with Java's block in its void, but it didnt",
            world.full(
                new Settled(world).state().row(
                    world.intern(
                        new Value('O', "Φ.app.φ.α1", "Φ.app.φ", 1L | Formation.RHO, "", -1, 0)
                    ),
                    "b"
                ).get(0)
            ),
            Matchers.equalTo("Φ.block@java{ρ}")
        );
    }

    @Test
    void notesCopyWhoseRhoOnlyJavaFills(@Mktmp final Path temp) throws IOException {
        final World world = new Sources(temp)
            .with("jobs.eo", "[] > jobs", "  [] > run /Q.number", "    ? > job")
            .with("app.eo", "[] > app", "  jobs.run > @", "    [^]", "      42 > @")
            .world();
        MatcherAssert.assertThat(
            "a copy handed to an atom with a declared ^ nobody binds must be noted, but it wasnt",
            new Settled(world).state().handedToJava(
                world.intern(
                    new Value('O', "Φ.app.φ.α0", "Φ.app.φ.α0", 0L, "", -1, 0)
                )
            ),
            Matchers.is(true)
        );
    }
}
