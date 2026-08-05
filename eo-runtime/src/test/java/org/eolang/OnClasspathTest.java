/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link OnClasspath}.
 * @since 0.62
 */
final class OnClasspathTest {

    @Test
    void findsExistingClass() {
        MatcherAssert.assertThat(
            "A class that exists must be reported as present, but it wasn't",
            OnClasspath.has("org.eolang.PhDefault"),
            Matchers.is(true)
        );
    }

    @Test
    void doesNotFindMissingClass() {
        MatcherAssert.assertThat(
            "A class that is absent must be reported as missing, but it wasn't",
            OnClasspath.has("org.eolang.ThereIsDefinitelyNoSuchClass"),
            Matchers.is(false)
        );
    }

    @Test
    void returnsSameAnswerOnRepeatedLookup() {
        final String cls = "org.eolang.PhNest";
        MatcherAssert.assertThat(
            "The cached lookup must agree with itself on repeat, but it didn't",
            OnClasspath.has(cls),
            Matchers.is(OnClasspath.has(cls))
        );
    }

    @Test
    void doesNotRunStaticInitializerWhileProbing() {
        MatcherAssert.assertThat(
            "a presence probe must not execute the target class's static initializer",
            OnClasspath.has(
                "org.eolang.OnClasspathTest$ExplodingOnInit"
            ),
            Matchers.is(true)
        );
        MatcherAssert.assertThat(
            "the static initializer must still not have run after the probe; "
                + "reading this flag on a class OTHER than the probed one avoids "
                + "triggering initialization as a side effect of the check itself",
            Flag.initialized,
            Matchers.is(false)
        );
    }

    /**
     * Records, from outside {@link ExplodingOnInit}, whether its initializer ran.
     *
     * <p>A test that read {@code ExplodingOnInit}'s own field would itself trigger
     * the very initialization it is trying to detect, per JLS class-initialization
     * rules. Recording the fact on a separate, untouched class avoids that.</p>
     *
     * @since 0.74.0
     */
    static final class Flag {

        /**
         * Set to TRUE by {@link ExplodingOnInit}'s static initializer, if it ever runs.
         */
        static boolean initialized;

        private Flag() {
        }
    }

    /**
     * A class whose static initializer flips {@link Flag#initialized}, to detect
     * eager class initialization.
     * @since 0.74.0
     */
    static final class ExplodingOnInit {

        static {
            Flag.initialized = true;
        }
    }
}
