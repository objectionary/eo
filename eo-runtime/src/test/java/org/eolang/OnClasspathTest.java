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
}
