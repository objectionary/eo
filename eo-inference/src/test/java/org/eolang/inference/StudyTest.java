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
        final World world = new Sources(temp)
            .with("kite.eo", "[wind] > kite", "  wind > @")
            .world();
        MatcherAssert.assertThat(
            "the copy as written of a formation nobody calls must be kept, but it wasnt",
            new Settled(world).state().kept(
                world.intern(new Value('O', "Φ.kite", "Φ.kite", Formation.RHO, "", -1, 0))
            ),
            Matchers.is(true)
        );
    }

    @Test
    void doesNotKeepCopyAsWrittenOfFormationSomeoneCalls(@Mktmp final Path temp)
        throws IOException {
        final World world = new Sources(temp)
            .with("kite.eo", "[wind] > kite", "  wind > @")
            .with("beach.eo", "[] > beach", "  kite 5 > @")
            .world();
        MatcherAssert.assertThat(
            "the copy as written of a formation someone calls must not be kept, but it was",
            new Settled(world).state().kept(
                world.intern(new Value('O', "Φ.kite", "Φ.kite", Formation.RHO, "", -1, 0))
            ),
            Matchers.is(false)
        );
    }

    @Test
    void givesStudyCopyInsideRealCopyAround(@Mktmp final Path temp) throws IOException {
        final World world = new Sources(temp)
            .with("kite.eo", "[wind] > kite", "  [knot] > tail", "    knot > @", "  wind > @")
            .with("beach.eo", "[] > beach", "  kite 5 > @")
            .world();
        MatcherAssert.assertThat(
            "a formation nobody calls must get a study copy inside the real copy around it, but it didnt",
            new Settled(world).state().kept(
                world.intern(
                    new Value(
                        'O', "Φ.kite.tail",
                        String.format(
                            "in:%s",
                            world.full(
                                world.intern(
                                    new Value(
                                        'O', "Φ.kite", "Φ.beach.φ", 1L | Formation.RHO, "", -1, 0
                                    )
                                )
                            )
                        ),
                        Formation.RHO, "", -1, 0
                    )
                )
            ),
            Matchers.is(true)
        );
    }
}
