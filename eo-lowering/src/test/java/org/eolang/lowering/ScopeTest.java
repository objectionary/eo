/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Tests of the class {@link Scope}.
 *
 * @since 0.64.0
 */
final class ScopeTest {

    @Test
    void coversALocatorItIncludes() {
        MatcherAssert.assertThat(
            "a locator that the filter of the included matches must be covered, but it isnt",
            new Scope("Φ\\.qw\\..*", "Φ\\.zz").covers("Φ.qw.a🌵7-3"),
            Matchers.is(true)
        );
    }

    @Test
    void leavesOutALocatorItDoesntInclude() {
        MatcherAssert.assertThat(
            "a locator that the filter of the included doesnt match must not be covered, but it is",
            new Scope("Φ\\.qw", "Φ\\.zz").covers("Φ.qw.tail"),
            Matchers.is(false)
        );
    }

    @Test
    void leavesOutALocatorItExcludes() {
        MatcherAssert.assertThat(
            "a locator that the filter of the excluded matches must not be covered, but it is",
            new Scope(".*", ".*\\.printf").covers("Φ.string.printf"),
            Matchers.is(false)
        );
    }
}
