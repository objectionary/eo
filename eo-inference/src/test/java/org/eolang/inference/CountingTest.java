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
 * Test case for {@link Counting}.
 *
 * @since 0.76.0
 */
@ExtendWith(MktmpResolver.class)
final class CountingTest {

    @Test
    void countsCopyWithNoEmptyVoid(@Mktmp final Path temp) throws IOException {
        final State state = CountingTest.settled(temp);
        MatcherAssert.assertThat(
            "a copy with no empty void must count, but it didnt",
            state.counting.counts(
                state.values.intern(new Value('O', "Φ.app", "Φ.app", Formation.RHO, "", -1, 0))
            ),
            Matchers.is(true)
        );
    }

    @Test
    void doesNotCountCopyWithEmptyVoidNobodyUses(@Mktmp final Path temp) throws IOException {
        final State state = CountingTest.settled(temp);
        MatcherAssert.assertThat(
            "a copy with an empty void that nobody uses must not count, but it did",
            state.counting.counts(
                state.values.intern(new Value('O', "Φ.box", "Φ.box", Formation.RHO, "", -1, 0))
            ),
            Matchers.is(false)
        );
    }

    @Test
    void countsCopyCodeReadsFromOutside(@Mktmp final Path temp) throws IOException {
        final State state = CountingTest.settled(temp);
        MatcherAssert.assertThat(
            "a copy read from outside must count, but it didnt",
            state.counting.counts(
                state.values.intern(
                    new Value('O', "Φ.box", "Φ.app.φ.ρ", 1L | Formation.RHO, "", -1, 0)
                )
            ),
            Matchers.is(true)
        );
    }

    @Test
    void countsValueThatIsNoCopy(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "a value that is not a copy must count, but it didnt",
            CountingTest.settled(temp).counting.counts(Values.TERM),
            Matchers.is(true)
        );
    }

    private static State settled(final Path temp) throws IOException {
        return new Sources(
            temp,
            "box.eo", "[item other] > box\n  item > @\n  other > size\n",
            "app.eo", "[] > app\n  (box 5).size > @\n  box 1 2 > full\n"
        ).settled();
    }
}
