/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.yegor256.Together;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;
import org.cactoos.Input;
import org.cactoos.io.InputOf;
import org.cactoos.map.MapOf;
import org.cactoos.text.TextOf;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.RepeatedTest;
import org.junit.jupiter.api.Test;

/**
 * Tests for {@link OyCached}.
 *
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
    void goesToOriginWhenCacheDoesNotHaveIt() throws Exception {
        MatcherAssert.assertThat(
            "The retrieved input does not match with expected",
            new TextOf(
                new OyCached(
                    new Objectionary.Fake(nme -> new InputOf("Hello from origin!")),
                    new MapOf<>()
                ).get("foo")
            ).asString(),
            Matchers.equalTo("Hello from origin!")
        );
    }

    @Test
    void savesInCacheWhenCacheDoesNotHaveIt() throws Exception {
        final String key = "jeff";
        final Map<String, Input> cache = new MapOf<>();
        new OyCached(new Objectionary.Fake(nme -> new InputOf("[] > jeff")), cache).get(key);
        MatcherAssert.assertThat(
            "The retrieved content from origin should be saved in cache, but it was not",
            new TextOf(cache.get(key)).asString(),
            Matchers.equalTo("[] > jeff")
        );
    }

    @Test
    void downloadsCachedProgramOnlyOnce() throws Exception {
        final AtomicInteger downloads = new AtomicInteger(0);
        final Objectionary objectionary = new OyCached(
            new Objectionary.Fake(
                nme -> () -> {
                    downloads.incrementAndGet();
                    return new ByteArrayInputStream(
                        "[] > remote".getBytes(StandardCharsets.UTF_8)
                    );
                }
            )
        );
        new TextOf(objectionary.get("remote")).asString();
        new TextOf(objectionary.get("remote")).asString();
        MatcherAssert.assertThat(
            "The cached program must be downloaded once, while every read goes to the origin",
            downloads.get(),
            Matchers.equalTo(1)
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

    @RepeatedTest(10)
    void checksPresenceInConcurrentEnvironment() {
        final AtomicInteger calls = new AtomicInteger(0);
        final Objectionary objectionary = new OyCached(
            new Objectionary.Fake(
                nme -> new InputOf("[] > foo"),
                nme -> {
                    calls.incrementAndGet();
                    return true;
                },
                nme -> false
            )
        );
        new Together<>(30, thread -> objectionary.contains("parallel")).asList();
        final int expected = 1;
        MatcherAssert.assertThat(
            String.format("Original objectionary should be asked only %d time", expected),
            calls.get(),
            Matchers.equalTo(expected)
        );
    }

    @Test
    void propagatesOriginFailureFromContainsAsIoException() {
        final IOException failure = new IOException("origin failed");
        MatcherAssert.assertThat(
            "the original IOException must stay in the cause chain",
            Assertions.assertThrows(
                IOException.class,
                () -> new OyCached(new FailingObjectionary(failure)).contains("foo"),
                "a failure from the origin objectionary must surface as IOException"
            ).getCause().getCause(),
            Matchers.sameInstance(failure)
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
        MatcherAssert.assertThat(
            "the original IOException must stay in the cause chain",
            Assertions.assertThrows(
                IOException.class,
                () -> new OyCached(new FailingObjectionary(failure)).get("foo"),
                "a failure from the origin objectionary must surface as IOException"
            ).getCause().getCause(),
            Matchers.sameInstance(failure)
        );
    }

    @Test
    void propagatesOriginFailureFromIsDirectoryAsIoException() {
        final IOException failure = new IOException("origin failed");
        MatcherAssert.assertThat(
            "the original IOException must stay in the cause chain",
            Assertions.assertThrows(
                IOException.class,
                () -> new OyCached(new FailingObjectionary(failure)).isDirectory("foo"),
                "a failure from the origin objectionary must surface as IOException"
            ).getCause().getCause(),
            Matchers.sameInstance(failure)
        );
    }
}
