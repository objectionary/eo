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
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link Pool}.
 *
 * @since 0.76.0
 */
@ExtendWith(MktmpResolver.class)
final class PoolTest {

    @Test
    void waitsWhileNothingIsInPool(@Mktmp final Path temp) throws IOException {
        final State state = PoolTest.state(temp);
        state.pool.read(PoolTest.java(state), "Φ.os.status", "code", "Φ.app.φ", "");
        MatcherAssert.assertThat(
            "a void read with nothing in its pool must wait for change a, but it didnt",
            state.pool.waiting(),
            Matchers.contains(List.of("Φ.os.status", "code"))
        );
    }

    @Test
    void readsWhatCodePutIntoPool(@Mktmp final Path temp) throws IOException {
        final State state = PoolTest.state(temp);
        state.pool.fill("Φ.os.status", "code", new int[] {Values.DATA});
        MatcherAssert.assertThat(
            "a void must read what EO code put into its pool, but it didnt",
            state.pool.read(PoolTest.java(state), "Φ.os.status", "code", "Φ.app.φ", "").array(),
            Matchers.equalTo(new int[] {Values.DATA})
        );
    }

    @Test
    void readsOpenVoidOnceDecidedJavaAloneFillsIt(@Mktmp final Path temp) throws IOException {
        final State state = PoolTest.state(temp);
        state.pool.decide(List.of(List.of("Φ.os.status", "code")));
        MatcherAssert.assertThat(
            "a void Java alone fills must read the decided open void, but it didnt",
            state.pool.read(PoolTest.java(state), "Φ.os.status", "code", "Φ.app.φ", "").array(),
            Matchers.equalTo(new int[] {Values.JGAP})
        );
    }

    private static int java(final State state) {
        return state.copies.fresh("Φ.os.status", Values.JBORN, Values.JAVA, "");
    }

    private static State state(final Path temp) throws IOException {
        final State state = new State(
            new Sources(
                temp,
                "os.eo", "[] > os\n  [] > check /Q.os.status\n  [code] > status\n    code > @\n"
            ).program(),
            new Names()
        );
        state.copies.seed();
        return state;
    }
}
