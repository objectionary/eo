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
 * Test case for {@link Telling}.
 *
 * @since 0.76.0
 */
@ExtendWith(MktmpResolver.class)
final class TellingTest {

    @Test
    void namesDecoratorByWhatItsAtGives(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "a place that reads a decorator must be named by what its @ gives, but it wasnt",
            TellingTest.told(temp).sets().get("Φ.app.φ"),
            Matchers.equalTo(Map.of("Φ.number", 1))
        );
    }

    @Test
    void countsCopiesThatSayEachSet(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "each set must be counted by the copies that say it, but it wasnt",
            TellingTest.told(temp).sets().get("Φ.box.item"),
            Matchers.equalTo(Map.of("Φ.number", 1, "Φ.string", 1))
        );
    }

    @Test
    void tellsVoidNobodyFillsAsOpen(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "a void no call fills must be told as an open void, but it wasnt",
            TellingTest.told(temp).sets().get("Φ.box.spare"),
            Matchers.equalTo(Map.of("?open-void", 2))
        );
    }

    @Test
    void tellsWhatCopiesOfFormationReduceTo(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "what the copies of a decorator reduce to must be told, but it wasnt",
            TellingTest.told(temp).reduction().get("Φ.five"),
            Matchers.equalTo(Map.of("Φ.number", 1))
        );
    }

    private static Told told(final Path temp) throws IOException {
        final State state = new Sources(temp).with(
            "five.eo",
            "[] > five",
            "  5 > @"
        ).with(
            "box.eo",
            "[item spare] > box",
            "  item > @",
            "  spare > other"
        ).with(
            "app.eo",
            "[] > app",
            "  five > @",
            "  (box 1).other > one",
            "  (box \"a\").other > word"
        ).settled();
        state.end().step();
        return new Telling(state).told();
    }
}
