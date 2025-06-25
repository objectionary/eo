/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.io.IOException;
import java.util.Map;
import org.cactoos.Input;
import org.cactoos.io.InputOf;
import org.cactoos.map.MapOf;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Tests for {@link OyCached}.
 * @since 0.56.10
 */
final class OyCachedTest {

    @Test
    void returnsFromCacheWhileOriginDoesNotHaveIt() throws IOException {
        final String key = "foo";
        final InputOf expected = new InputOf("bar");
        MatcherAssert.assertThat(
            String.format(
                "The input was not retrieved by the '%s' key from cache",
                key
            ),
            new OyCached(
                new Objectionary.Fake(),
                new MapOf<>(key, expected)
            ).get(key),
            Matchers.equalTo(expected)
        );
    }

    @Test
    void goesToOriginWhenCacheDoesNotHaveIt() throws IOException {
        final Input expected = new InputOf("Hello from origin!");
        MatcherAssert.assertThat(
            "The retrieved input does not match with expected",
            new OyCached(
                new Objectionary.Fake(nme -> expected), new MapOf<>()
            ).get("foo"),
            Matchers.equalTo(expected)
        );
    }

    @Test
    void savesInCacheWhenCacheDoesNotHaveIt() throws IOException {
        final String key = "jeff";
        final Input value = new InputOf("[] > jeff");
        final Map<String, Input> cache = new MapOf<>();
        new OyCached(new Objectionary.Fake(nme -> value), cache).get(key);
        MatcherAssert.assertThat(
            "The retrieved content from origin should be saved in cache, but it was not",
            cache,
            Matchers.hasEntry(key, value)
        );
    }
}
