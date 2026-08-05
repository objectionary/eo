/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.yegor256.Together;
import java.io.IOException;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;
import org.cactoos.Input;
import org.cactoos.io.InputOf;
import org.cactoos.iterable.IterableOf;
import org.cactoos.map.MapOf;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.RepeatedTest;
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

    @RepeatedTest(10)
    void cachesInConcurrentEnvironment() {
        final AtomicInteger calls = new AtomicInteger(0);
        final Input content = new InputOf("[] > foo");
        final Objectionary objectionary = new OyCached(
            new Objectionary.Fake(
                nme -> {
                    calls.incrementAndGet();
                    return content;
                }
            )
        );
        new Together<>(30, thread -> objectionary.get("parallel")).asList();
        final int expected = 1;
        MatcherAssert.assertThat(
            String.format("Original objectionary should be called only %d time", expected),
            calls.get(),
            Matchers.equalTo(expected)
        );
    }

    @Test
    void checksIsDirectoryWithEmptyCache() throws IOException {
        MatcherAssert.assertThat(
            "The directory should not be found in origin, but it was",
            new OyCached(
                new Objectionary.Fake(), new MapOf<>()
            ).isDirectory("xxx"),
            Matchers.is(false)
        );
    }

    @Test
    void checksIsDirectoryWithExistingInCache() throws IOException {
        final String key = "abc";
        MatcherAssert.assertThat(
            "The directory should be found in cache, but it was not",
            new OyCached(
                new Objectionary.Fake(
                    nme -> new InputOf("[] > abc")
                ),
                new MapOf<>(),
                new MapOf<>(key, true)
            ).isDirectory(key),
            Matchers.is(true)
        );
    }

    @Test
    void checksIsDirectoryWithNotExistingInCache() throws IOException {
        MatcherAssert.assertThat(
            "The directory should not be found in cache, but it was",
            new OyCached(
                new Objectionary.Fake(
                    nme -> new InputOf("[] > jeff")
                ),
                new MapOf<>(),
                new MapOf<>("jeff", true)
            ).isDirectory("not-in-cache"),
            Matchers.is(false)
        );
    }

    @Test
    void propagatesOriginFailureFromGetAsIoException() {
        final IOException failure = new IOException("origin failed");
        final IOException caught = Assertions.assertThrows(
            IOException.class,
            () -> new OyCached(new OyCachedTest.Failing(failure)).get("foo"),
            "a failure from the origin objectionary must surface as IOException, "
                + "not IllegalStateException"
        );
        MatcherAssert.assertThat(
            "the original IOException must reach the caller unwrapped",
            caught,
            Matchers.sameInstance(failure)
        );
    }

    @Test
    void propagatesOriginFailureFromIsDirectoryAsIoException() {
        final IOException failure = new IOException("origin failed");
        final IOException caught = Assertions.assertThrows(
            IOException.class,
            () -> new OyCached(new OyCachedTest.Failing(failure)).isDirectory("foo"),
            "a failure from the origin objectionary must surface as IOException, "
                + "not IllegalStateException"
        );
        MatcherAssert.assertThat(
            "the original IOException must reach the caller unwrapped",
            caught,
            Matchers.sameInstance(failure)
        );
    }

    /**
     * An objectionary whose {@code get()} and {@code isDirectory()} always fail
     * with a given {@link IOException}.
     * @since 0.74.0
     */
    private static final class Failing implements Objectionary {

        /**
         * The failure to throw.
         */
        private final IOException failure;

        /**
         * Ctor.
         * @param cause The failure to throw
         */
        Failing(final IOException cause) {
            this.failure = cause;
        }

        @Override
        public Input get(final String name) throws IOException {
            throw this.failure;
        }

        @Override
        public boolean contains(final String name) {
            return false;
        }

        @Override
        public boolean isDirectory(final String name) throws IOException {
            throw this.failure;
        }

        @Override
        public Iterable<String> children(final String pkg) {
            return new IterableOf<>();
        }
    }
}
