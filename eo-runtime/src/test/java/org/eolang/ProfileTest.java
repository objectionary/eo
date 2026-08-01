/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.parallel.Isolated;

/**
 * Test case for {@link Profile}, the running one that {@link PhDefault}
 * feeds. The class runs alone, since the profile it reads is shared by
 * every object in the JVM.
 * @since 0.62
 */
@Isolated
final class ProfileTest {

    @Test
    void countsOneAllocationPerConstructedObject() {
        final long before = Profile.RUNNING.allocations();
        new PhDefault("org.eolang.bytes");
        MatcherAssert.assertThat(
            "constructing one object moved the running profile by other than one allocation",
            Profile.RUNNING.allocations() - before,
            Matchers.equalTo(1L)
        );
    }

    @Test
    void countsOneAllocationPerCopy() {
        final Phi phi = new PhDefault("org.eolang.bytes");
        final long before = Profile.RUNNING.allocations();
        phi.copy();
        MatcherAssert.assertThat(
            "copying one object moved the running profile by other than one allocation",
            Profile.RUNNING.allocations() - before,
            Matchers.equalTo(1L)
        );
    }

    @Test
    void countsOneDispatchPerTake() {
        final Phi phi = new PhDefault(new Attrs(new Attr("x", new AtVoid("x"))));
        final long before = Profile.RUNNING.dispatches();
        phi.take("x");
        MatcherAssert.assertThat(
            "taking one attribute moved the running profile by other than one dispatch",
            Profile.RUNNING.dispatches() - before,
            Matchers.equalTo(1L)
        );
    }
}
