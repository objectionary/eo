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
        final State state = RowsTest.state(temp);
        final int cup = RowsTest.cup(state, 0L);
        state.rows().gather(cup, "tea", new int[] {Values.DATA});
        state.rows().gather(cup, "tea", new int[] {Values.TERM, Values.DATA});
        MatcherAssert.assertThat(
            "a row must hold every value gathered into it, each once, but it didnt",
            state.rows().row(cup, "tea").frozen(),
            Matchers.equalTo(new Ids(Values.DATA, Values.TERM))
        );
    }

    @Test
    void joinsBlankCopyPutIntoRho(@Mktmp final Path temp) throws IOException {
        final State state = RowsTest.state(temp);
        final int cup = RowsTest.cup(state, Formation.RHO);
        state.rows().gather(
            cup, "ρ", new int[] {state.copies().fresh("Φ.number", new Site("Φ.x", ""), Values.ROOT)}
        );
        MatcherAssert.assertThat(
            "a blank copy put into ρ must join the shared one, but it didnt",
            state.rows().row(cup, "ρ").frozen(),
            Matchers.equalTo(new Ids(state.copies().shared("Φ.number")))
        );
    }

    @Test
    void stampsCopyWithNoRhoOntoHolder(@Mktmp final Path temp) throws IOException {
        final State state = RowsTest.state(temp);
        MatcherAssert.assertThat(
            "a copy with no ρ must be stamped onto its holder at the place of the read, but it wasnt",
            state.values().full(
                state.rows().stamp(
                    RowsTest.cup(state, 0L), RowsTest.cup(state, Formation.RHO),
                    new Site("Φ.tray.ρ", "")
                )
            ),
            Matchers.equalTo("Φ.cup@Φ.tray.ρ{ρ}")
        );
    }

    @Test
    void leavesCopyWithRhoAsItIs(@Mktmp final Path temp) throws IOException {
        final State state = RowsTest.state(temp);
        final int cup = RowsTest.cup(state, Formation.RHO);
        MatcherAssert.assertThat(
            "a copy that has its ρ must stay as it is when read out of a row, but it didnt",
            state.rows().stamp(cup, Values.ROOT, new Site("Φ.tray.ρ", "")),
            Matchers.equalTo(cup)
        );
    }

    private static State state(final Path temp) throws IOException {
        final State state = new State(
            new Sources(temp).with("cup.eo", "[tea] > cup", "  tea > @").program(), new Names()
        );
        state.copies().seed();
        return state;
    }

    private static int cup(final State state, final long bound) {
        return state.values().intern(new Value('O', "Φ.cup", "Φ.cup", bound, "", -1, 0));
    }
}
