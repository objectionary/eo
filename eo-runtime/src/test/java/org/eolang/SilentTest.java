/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Silent}.
 * @since 0.62
 */
final class SilentTest {

    @Test
    void forgetsEverythingItIsTold() {
        final Statistics stats = new Silent();
        stats.allocate();
        stats.dispatch();
        MatcherAssert.assertThat(
            "a silenced statistics remembered what it was told",
            stats.allocations() + stats.dispatches(),
            Matchers.equalTo(0L)
        );
    }
}
