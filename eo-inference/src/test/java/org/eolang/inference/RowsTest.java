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
 * Test case for {@link Rows}.
 *
 * @since 0.76.0
 */
@ExtendWith(MktmpResolver.class)
final class RowsTest {

    @Test
    void gathersValuesIntoRow(@Mktmp final Path temp) throws IOException {
        final World world = RowsTest.world(temp);
        final State state = RowsTest.state(world);
        final int cup = RowsTest.cup(world, 0L);
        state.gather(cup, "tea", new int[] {Values.DATA});
        state.gather(cup, "tea", new int[] {Values.TERM, Values.DATA});
        MatcherAssert.assertThat(
            "a row must hold every value gathered into it, each once, but it didnt",
            state.row(cup, "tea").frozen(),
            Matchers.hasToString(new Ids(Values.DATA, Values.TERM).toString())
        );
    }

    @Test
    void joinsBlankCopyPutIntoRho(@Mktmp final Path temp) throws IOException {
        final World world = RowsTest.world(temp);
        final State state = RowsTest.state(world);
        final int cup = RowsTest.cup(world, Formation.RHO);
        state.gather(
            cup, "ρ", new int[] {state.fresh("Φ.number", new Site("Φ.x", ""), Values.ROOT)}
        );
        MatcherAssert.assertThat(
            "a blank copy put into ρ must join the shared one, but it didnt",
            state.row(cup, "ρ").frozen(),
            Matchers.hasToString(new Ids(state.shared("Φ.number")).toString())
        );
    }

    @Test
    void stampsCopyWithNoRhoOntoHolder(@Mktmp final Path temp) throws IOException {
        final World world = RowsTest.world(temp);
        MatcherAssert.assertThat(
            "a copy with no ρ must be stamped onto its holder at the place of the read, but it wasnt",
            world.full(
                RowsTest.state(world).stamp(
                    RowsTest.cup(world, 0L), RowsTest.cup(world, Formation.RHO),
                    new Site("Φ.tray.ρ", "")
                )
            ),
            Matchers.equalTo("Φ.cup@Φ.tray.ρ{ρ}")
        );
    }

    @Test
    void leavesCopyWithRhoAsItIs(@Mktmp final Path temp) throws IOException {
        final World world = RowsTest.world(temp);
        final int cup = RowsTest.cup(world, Formation.RHO);
        MatcherAssert.assertThat(
            "a copy that has its ρ must stay as it is when read out of a row, but it didnt",
            RowsTest.state(world).stamp(cup, Values.ROOT, new Site("Φ.tray.ρ", "")),
            Matchers.equalTo(cup)
        );
    }

    private static State state(final World world) {
        final State state = new State(world);
        state.seed();
        return state;
    }

    private static World world(final Path temp) throws IOException {
        return new Sources(temp).with("cup.eo", "[tea] > cup", "  tea > @").world();
    }

    private static int cup(final World world, final long bound) {
        return world.intern(new Value('O', "Φ.cup", "Φ.cup", bound, "", -1, 0));
    }
}
