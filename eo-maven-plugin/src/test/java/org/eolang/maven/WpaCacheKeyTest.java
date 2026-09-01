/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.nio.file.Path;
import java.util.Collections;
import java.util.Map;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link WpaCacheKey}.
 * @since 0.73.4
 */
final class WpaCacheKeyTest {

    @Test
    void changesWhenWpaVersionChanges() {
        final Map<String, Path> paths = Collections.emptyMap();
        MatcherAssert.assertThat(
            "An upgrade of the wpa artifact must invalidate the cache key even when every other input stays the same",
            new WpaCacheKey(paths, Collections.emptyList(), false, "0.1.0").get(),
            Matchers.not(
                Matchers.equalTo(
                    new WpaCacheKey(paths, Collections.emptyList(), false, "0.1.1").get()
                )
            )
        );
    }

    @Test
    void staysTheSameWhenNothingChanges() {
        final Map<String, Path> paths = Collections.emptyMap();
        MatcherAssert.assertThat(
            "The same inputs must produce the same cache key",
            new WpaCacheKey(paths, Collections.emptyList(), false, "0.1.1").get(),
            Matchers.equalTo(
                new WpaCacheKey(paths, Collections.emptyList(), false, "0.1.1").get()
            )
        );
    }
}
