/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.Base64;
import java.util.concurrent.atomic.AtomicInteger;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test for {@link Cache}.
 * @since 0.60
 */
@ExtendWith(MktmpResolver.class)
final class CacheTest {

    @Test
    void compilesSourceAndAddsToCache(@Mktmp final Path temp) throws Exception {
        final var base = temp.resolve("cache");
        Files.createDirectories(base);
        final var source = temp.resolve("source.eo");
        Files.writeString(source, "[] > main\n  (stdout \"Hello, EO!\") > @\n");
        final var target = temp.resolve("target.xmir");
        final var tail = source.getFileName();
        final String content = "compiled";
        new Cache(base, p -> content).apply(source, target, tail);
        MatcherAssert.assertThat(
            "Target file must be created from source",
            Files.readString(target),
            Matchers.equalTo(content)
        );
        MatcherAssert.assertThat(
            "Cache file must be created",
            Files.exists(base.resolve(tail)),
            Matchers.is(true)
        );
        MatcherAssert.assertThat(
            "Hash file must be created",
            Files.exists(base.resolve(String.format("%s.sha256", tail))),
            Matchers.is(true)
        );
    }

    @Test
    void readsFromCacheWhenUnchanged(@Mktmp final Path temp) throws Exception {
        final var base = temp.resolve("cache-directory");
        Files.createDirectories(base);
        final var source = temp.resolve("stdin.eo");
        Files.writeString(source, "[] > main\n  (stdout \"Hello, EO!\") > @\n");
        final var target = temp.resolve("stdin.xmir");
        final var counter = new AtomicInteger(0);
        final var cache = new Cache(
            base,
            p -> String.format("stdin %d", counter.incrementAndGet())
        );
        final Path tail = source.getFileName();
        cache.apply(source, target, tail);
        MatcherAssert.assertThat(
            "Compilation should happen only once",
            counter.get(),
            Matchers.equalTo(1)
        );
        cache.apply(source, target, tail);
        MatcherAssert.assertThat(
            "Compilation should not happen again",
            counter.get(),
            Matchers.equalTo(1)
        );
    }

    @Test
    void compilesAgainWhenChanged(@Mktmp final Path temp) throws Exception {
        final var base = temp.resolve("cache-base-dir");
        Files.createDirectories(base);
        final var source = temp.resolve("stdout.eo");
        Files.writeString(source, "[] > main\n  (stdout \"Hello, EO!\") > @\n");
        final var target = temp.resolve("stdout.xmir");
        final var counter = new AtomicInteger(0);
        final var cache = new Cache(
            base,
            p -> String.format("compiled %d", counter.incrementAndGet())
        );
        cache.apply(source, target, source.getFileName());
        MatcherAssert.assertThat(
            "Compilation should happen once",
            counter.get(),
            Matchers.equalTo(1)
        );
        Files.writeString(source, "[] > main\n  (stdout \"Hello, EO! Modified\") > @\n");
        cache.apply(source, target, source.getFileName());
        MatcherAssert.assertThat(
            "Compilation should happen again after source change",
            counter.get(),
            Matchers.equalTo(2)
        );
    }

    @Test
    void compilesIfHashExistsButCacheMissing(@Mktmp final Path temp) throws Exception {
        final var base = temp.resolve("cache-root");
        Files.createDirectories(base);
        final var source = temp.resolve("data.eo");
        Files.writeString(source, "[] > main\n  (stdout \"Data EO\") > @\n");
        final var target = temp.resolve("data.xmir");
        final var counter = new AtomicInteger(0);
        final var cache = new Cache(
            base,
            p -> String.format("data %d", counter.incrementAndGet())
        );
        final Path tail = source.getFileName();
        cache.apply(source, target, tail);
        MatcherAssert.assertThat(
            "Compilation should happen once",
            counter.get(),
            Matchers.equalTo(1)
        );
        Files.delete(base.resolve(tail));
        cache.apply(source, target, tail);
        MatcherAssert.assertThat(
            "Compilation should happen again after cache deletion",
            counter.get(),
            Matchers.equalTo(2)
        );
    }

    @Test
    void writesCorrectShaHash(@Mktmp final Path temp) throws IOException, NoSuchAlgorithmException {
        final var base = temp.resolve("cache");
        Files.createDirectories(base);
        final var source = temp.resolve("message.txt");
        final String msg = "hello";
        final Charset encoding = StandardCharsets.UTF_8;
        Files.writeString(source, msg, encoding);
        final var target = temp.resolve("out.txt");
        final var tail = source.getFileName();
        final var cache = new Cache(base, p -> "compiled");
        cache.apply(source, target, tail);
        MatcherAssert.assertThat(
            "SHA-256 hash file has incorrect content",
            Files.readString(base.resolve(String.format("%s.sha256", tail)), encoding),
            Matchers.equalTo(
                Base64.getEncoder().encodeToString(
                    MessageDigest.getInstance("SHA-256").digest(msg.getBytes(encoding))
                )
            )
        );
    }
}
