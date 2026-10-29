/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link Copies}.
 *
 * @since 0.76.0
 */
@ExtendWith(MktmpResolver.class)
final class CopiesTest {

    @Test
    void sharesOneNumberAmongCallsAtDifferentPlaces(@Mktmp final Path temp)
        throws IOException {
        final State state = new State(new Sources(temp).program(), new Names());
        state.copies().seed();
        MatcherAssert.assertThat(
            "two numbers made at two places must be the one shared copy, but they werent",
            state.copies().copy(
                new Value("Φ.number", new Site("Φ.kiosk.φ", ""), 1L | Formation.RHO), Map.of()
            ),
            Matchers.equalTo(
                state.copies().copy(
                    new Value("Φ.number", new Site("Φ.till.φ.α0", ""), 1L | Formation.RHO),
                    Map.of()
                )
            )
        );
    }

    @Test
    void givesBlankCopyToBareNameReadOffRoot(@Mktmp final Path temp) throws IOException {
        final State state = new State(new Sources(temp).program(), new Names());
        state.copies().seed();
        MatcherAssert.assertThat(
            "the bare name read off Φ must be the blank copy, but it wasnt",
            state.values().get(
                state.copies().fresh("Φ.string", new Site("Φ.shop.φ.ρ", ""), Values.ROOT)
            ).birth(),
            Matchers.equalTo("blank")
        );
    }

    @Test
    void joinsBlankCopyToSharedOne(@Mktmp final Path temp) throws IOException {
        final State state = new State(new Sources(temp).program(), new Names());
        state.copies().seed();
        MatcherAssert.assertThat(
            "the blank copy must join the shared one, but it didnt",
            state.copies().joined(
                state.copies().fresh("Φ.bytes", new Site("Φ.box.φ", ""), Values.ROOT)
            ),
            Matchers.equalTo(state.copies().shared("Φ.bytes"))
        );
    }

    @Test
    void keepsNoCanonWhenProgramLacksKindOfIt(@Mktmp final Path temp) throws IOException {
        final State state = new State(
            new Sources(temp).program(),
            new Names(List.of("Φ.bytes", "Φ.money"), List.of(), List.of())
        );
        state.copies().seed();
        MatcherAssert.assertThat(
            "a program that lacks a kind of the canon must have no canon, but it had one",
            state.copies().canon("Φ.bytes"),
            Matchers.is(false)
        );
    }

    @Test
    void namesCopyByWhatItIsMadeOf(@Mktmp final Path temp) throws IOException {
        final State state = new State(
            new Sources(temp).with("cup.eo", "[tea milk] > cup", "  tea > @").program(), new Names()
        );
        state.copies().seed();
        MatcherAssert.assertThat(
            "a copy made twice of the same things must be one copy, but it wasnt",
            state.copies().copy(
                new Value("Φ.cup", new Site("Φ.tray.φ", "Φ.t"), 1L),
                Map.of("tea", new int[] {Values.DATA})
            ),
            Matchers.equalTo(
                state.copies().copy(new Value("Φ.cup", new Site("Φ.tray.φ", "Φ.t"), 1L), Map.of())
            )
        );
    }
}
