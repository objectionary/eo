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
 * Test case for {@link Calls}.
 *
 * @since 0.76.0
 */
@ExtendWith(MktmpResolver.class)
final class CallsTest {

    @Test
    void failsCallWithMoreArgumentsThanVoids(@Mktmp final Path temp) throws IOException {
        final World world = CallsTest.world(temp);
        MatcherAssert.assertThat(
            "a call with more arguments than empty voids must fail, but it didnt",
            new Settled(world).state().evaluate("Φ.app.many", CallsTest.app(world)),
            Matchers.equalTo(new Ids(Values.TERM))
        );
    }

    @Test
    void answersArgumentOfObjectWhoseAtIsItsVoid(@Mktmp final Path temp) throws IOException {
        final World world = CallsTest.world(temp);
        MatcherAssert.assertThat(
            "a call on an object whose @ is its void must answer the argument, but it didnt",
            world.value(
                new Settled(world).state().evaluate("Φ.app.echoed", CallsTest.app(world)).get(0)
            ).form(),
            Matchers.equalTo("Φ.number")
        );
    }

    @Test
    void landsArgumentInVoidItFills(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "an argument must land in the void it fills, but it didnt",
            new Settled(CallsTest.world(temp)).state().lands(),
            Matchers.hasEntry("Φ.app.φ.α1", "lid")
        );
    }

    private static int app(final World world) {
        return world.intern(new Value('O', "Φ.app", "Φ.app", Formation.RHO, "", -1, 0));
    }

    private static World world(final Path temp) throws IOException {
        return new Sources(temp)
            .with("jar.eo", "[jam lid] > jar", "  jam > @", "  lid > top")
            .with("echo.eo", "[sound] > echo", "  sound > @")
            .with("app.eo", "[] > app", "  jar 1 2 > @", "  jar 1 2 3 > many", "  echo 7 > echoed")
            .world();
    }
}
