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
import java.util.stream.IntStream;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test for {@link Cache}.
 *
 * @since 0.60
 */
@ExtendWith(MktmpResolver.class)
final class CacheTest {

    @Test
    void compilesSource(@Mktmp final Path temp) throws Exception {
        final Path base = temp.resolve("cache-folder");
        Files.createDirectories(base);
        final Path source = temp.resolve("source.eo");
        Files.writeString(source, String.format("[] > main%n  (stdout \"Hello, EO!\") > @%n"));
        final Path target = temp.resolve("target.xmir");
        final String content = "compiled";
        new Cache(base, p -> content).apply(source, target, source.getFileName());
        MatcherAssert.assertThat(
            "Target file must be created from source",
            Files.readString(target),
            Matchers.equalTo(content)
        );
    }

    @Test
    void compilesSourceAndAddsToCache(@Mktmp final Path temp) throws Exception {
        final Path base = temp.resolve("cache-root");
        Files.createDirectories(base);
        final Path source = temp.resolve("source.eo");
        Files.writeString(source, String.format("[] > main%n  (stdout \"Hello, EO!\") > @%n"));
        final Path tail = source.getFileName();
        new Cache(base, p -> "compiled").apply(source, temp.resolve("target.xmir"), tail);
        MatcherAssert.assertThat(
            "Cache file must be created and hash file must be created",
            Files.exists(base.resolve(tail))
                && Files.exists(base.resolve(CacheTest.marker(tail))),
            Matchers.is(true)
        );
    }

    @Test
    void readsFromCacheWhenUnchanged(@Mktmp final Path temp) throws Exception {
        final Path base = temp.resolve("cache-directory");
        Files.createDirectories(base);
        final Path source = temp.resolve("stdin.eo");
        Files.writeString(source, String.format("[] > main%n  (stdout \"Hello, EO!\") > @%n"));
        final Path target = temp.resolve("stdin.xmir");
        final AtomicInteger counter = new AtomicInteger(0);
        final Cache cache = new Cache(
            base,
            p -> String.format("stdin %d", counter.incrementAndGet())
        );
        final Path tail = source.getFileName();
        cache.apply(source, target, tail);
        cache.apply(source, target, tail);
        MatcherAssert.assertThat(
            "Compilation should not happen again",
            counter.get(),
            Matchers.equalTo(1)
        );
    }

    @Test
    void compilesAgainWhenChanged(@Mktmp final Path temp) throws Exception {
        final Path base = temp.resolve("cache-base-dir");
        Files.createDirectories(base);
        final Path source = temp.resolve("stdout.eo");
        Files.writeString(source, String.format("[] > main%n  (stdout \"Hello, EO!\") > @%n"));
        final Path target = temp.resolve("stdout.xmir");
        final AtomicInteger counter = new AtomicInteger(0);
        final Cache cache = new Cache(
            base,
            p -> String.format("compiled %d", counter.incrementAndGet())
        );
        cache.apply(source, target, source.getFileName());
        Files.writeString(
            source,
            String.format("[] > main%n  (stdout \"Hello, EO! Modified\") > @%n")
        );
        cache.apply(source, target, source.getFileName());
        MatcherAssert.assertThat(
            "Compilation should happen again after source change",
            counter.get(),
            Matchers.equalTo(2)
        );
    }

    @Test
    void compilesIfHashExistsButCacheMissing(@Mktmp final Path temp) throws Exception {
        final Path base = temp.resolve("cache-root");
        Files.createDirectories(base);
        final Path source = temp.resolve("data.eo");
        Files.writeString(source, String.format("[] > main%n  (stdout \"Data EO\") > @%n"));
        final Path target = temp.resolve("data.xmir");
        final AtomicInteger counter = new AtomicInteger(0);
        final Cache cache = new Cache(
            base,
            p -> String.format("data %d", counter.incrementAndGet())
        );
        final Path tail = source.getFileName();
        cache.apply(source, target, tail);
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
        final Path base = temp.resolve("cache-path");
        Files.createDirectories(base);
        final Path source = temp.resolve("message.txt");
        final String msg = "hello";
        final Charset encoding = StandardCharsets.UTF_8;
        Files.writeString(source, msg, encoding);
        final Path tail = source.getFileName();
        final Cache cache = new Cache(base, p -> "compiled");
        cache.apply(source, temp.resolve("out.txt"), tail);
        MatcherAssert.assertThat(
            "SHA-256 hash file has incorrect content",
            Files.readString(base.resolve(CacheTest.marker(tail)), encoding),
            Matchers.equalTo(
                Base64.getEncoder().encodeToString(
                    MessageDigest.getInstance("SHA-256").digest(msg.getBytes(encoding))
                )
            )
        );
    }

    @Test
    void generatesCorrectHashForLargeFile(
        @Mktmp final Path temp
    ) throws IOException, NoSuchAlgorithmException {
        final Path cache = temp.resolve("cache");
        Files.createDirectories(cache);
        final Path source = temp.resolve("large.txt");
        final int lines = 100_000;
        final StringBuilder builder = new StringBuilder(lines * 10);
        IntStream.range(0, lines).forEach(
            i -> builder.append("Line ").append(i).append('\n')
        );
        final String content = builder.toString();
        Files.writeString(source, content, StandardCharsets.UTF_8);
        final Path tail = source.getFileName();
        new Cache(cache, p -> content).apply(source, temp.resolve("out.txt"), tail);
        MatcherAssert.assertThat(
            "SHA-256 hash file has incorrect content for large file",
            Files.readString(
                cache.resolve(CacheTest.marker(tail)),
                StandardCharsets.UTF_8
            ),
            Matchers.equalTo(
                Base64.getEncoder().encodeToString(
                    MessageDigest.getInstance("SHA-256")
                        .digest(content.getBytes(StandardCharsets.UTF_8))
                )
            )
        );
    }

    @Test
    void generatesCorrectHashForTinyFile(
        @Mktmp final Path temp
    ) throws IOException, NoSuchAlgorithmException {
        final Path cache = temp.resolve("cache");
        Files.createDirectories(cache);
        final Path source = temp.resolve("tiny.txt");
        final String content = "x";
        Files.writeString(source, content, StandardCharsets.UTF_8);
        final Path tail = source.getFileName();
        new Cache(cache, p -> content).apply(source, temp.resolve("out.txt"), tail);
        MatcherAssert.assertThat(
            "SHA-256 hash file has incorrect content for tiny file",
            Files.readString(
                cache.resolve(CacheTest.marker(tail)),
                StandardCharsets.UTF_8
            ),
            Matchers.equalTo(CacheTest.hash(content))
        );
    }

    @Test
    void generatesCorrectHashForEntireFolderWithSeveralFiles(
        @Mktmp final Path temp
    ) throws IOException, NoSuchAlgorithmException {
        final Path cache = temp.resolve("cache");
        Files.createDirectories(cache);
        final Path source = temp.resolve("folder");
        Files.createDirectories(source);
        final String first = "file1 content";
        final String second = "file2 content";
        Files.writeString(source.resolve("file1.txt"), first, StandardCharsets.UTF_8);
        Files.writeString(source.resolve("file2.txt"), second, StandardCharsets.UTF_8);
        new Cache(cache, p -> String.format("%s%s", first, second)).apply(
            source,
            temp.resolve("out.txt"),
            source.getFileName()
        );
        MatcherAssert.assertThat(
            "SHA-256 hash file has incorrect content for folder with several files",
            Files.readString(
                cache.resolve("folder.sha256"),
                StandardCharsets.UTF_8
            ),
            Matchers.equalTo(new Sha(source).toString())
        );
    }

    @Test
    void generatesDifferentHashesForFoldersWithDifferentlyNamedIdenticalFiles(
        @Mktmp final Path temp
    ) throws IOException {
        final Path cache = temp.resolve("cache");
        Files.createDirectories(cache);
        final String content = "same content";
        final Path first = temp.resolve("dirA");
        Files.createDirectories(first);
        Files.writeString(first.resolve("original-name.eo"), content, StandardCharsets.UTF_8);
        new Cache(cache, p -> "compiled-a").apply(
            first, temp.resolve("out-a.txt"), first.getFileName()
        );
        final Path second = temp.resolve("dirB");
        Files.createDirectories(second);
        Files.writeString(
            second.resolve("completely-different-name.eo"), content, StandardCharsets.UTF_8
        );
        new Cache(cache, p -> "compiled-b").apply(
            second, temp.resolve("out-b.txt"), second.getFileName()
        );
        MatcherAssert.assertThat(
            "Directories with identical content but differently named files must produce different hashes",
            Files.readString(cache.resolve("dirA.sha256"), StandardCharsets.UTF_8),
            Matchers.not(
                Matchers.equalTo(
                    Files.readString(cache.resolve("dirB.sha256"), StandardCharsets.UTF_8)
                )
            )
        );
    }

    @Test
    void rejectsPayloadWriteFailure(@Mktmp final Path temp) throws IOException {
        final CacheTest.Corrupted state = CacheTest.corrupted(temp);
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> new Cache(state.base, p -> "v2").apply(state.source, state.target, state.tail),
            "a failure while writing the payload must be reported, not swallowed"
        );
    }

    @Test
    void leavesMarkerUntouchedAfterPayloadWriteFailure(
        @Mktmp final Path temp
    ) throws IOException, NoSuchAlgorithmException {
        final CacheTest.Corrupted state = CacheTest.corrupted(temp);
        CacheTest.attemptDoomedWrite(state);
        MatcherAssert.assertThat(
            "the marker must not point at a payload that was never written",
            Files.readString(
                state.base.resolve(CacheTest.marker(state.tail)), StandardCharsets.UTF_8
            ),
            Matchers.equalTo(CacheTest.hash("v1"))
        );
    }

    @Test
    void recompilesAfterPayloadWriteFailureInsteadOfServingStaleEntry(
        @Mktmp final Path temp
    ) throws IOException {
        final CacheTest.Corrupted state = CacheTest.corrupted(temp);
        CacheTest.attemptDoomedWrite(state);
        Files.delete(state.base.resolve(state.tail));
        final AtomicInteger counter = new AtomicInteger(0);
        new Cache(
            state.base, p -> String.format("v2-%d", counter.incrementAndGet())
        ).apply(state.source, state.target, state.tail);
        MatcherAssert.assertThat(
            "recovery after the failed write must recompile rather than serve a stale entry",
            counter.get(),
            Matchers.equalTo(1)
        );
    }

    private static CacheTest.Corrupted corrupted(final Path temp) throws IOException {
        final Path base = temp.resolve("cache-order");
        Files.createDirectories(base);
        final Path source = temp.resolve("order.eo");
        Files.writeString(source, "v1", StandardCharsets.UTF_8);
        final Path target = temp.resolve("order.xmir");
        final Path tail = source.getFileName();
        new Cache(base, p -> "v1").apply(source, target, tail);
        Files.writeString(source, "v2", StandardCharsets.UTF_8);
        final Path cached = base.resolve(tail);
        Files.delete(cached);
        Files.createDirectories(cached);
        return new CacheTest.Corrupted(base, source, target, tail);
    }

    private static void attemptDoomedWrite(final CacheTest.Corrupted state) {
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> new Cache(state.base, p -> "v2").apply(state.source, state.target, state.tail)
        );
    }

    private static String hash(final String content) throws NoSuchAlgorithmException {
        return Base64.getEncoder().encodeToString(
            MessageDigest.getInstance("SHA-256")
                .digest(content.getBytes(StandardCharsets.UTF_8))
        );
    }

    private static String marker(final Path tail) {
        return String.format("%s.sha256", tail);
    }

    /**
     * A cache whose payload path has been replaced with a directory, so
     * that a following write to it fails.
     *
     * @since 0.60
     */
    private static final class Corrupted {

        /**
         * Base cache directory.
         */
        private final Path base;

        /**
         * Source file, already changed since the last valid cache entry.
         */
        private final Path source;

        /**
         * Target file to write into.
         */
        private final Path target;

        /**
         * Tail path in cache.
         */
        private final Path tail;

        /**
         * Ctor.
         *
         * @param base Base cache directory
         * @param source Source file
         * @param target Target file
         * @param tail Tail path in cache
         */
        Corrupted(final Path base, final Path source, final Path target, final Path tail) {
            this.base = base;
            this.source = source;
            this.target = target;
            this.tail = tail;
        }
    }
}
