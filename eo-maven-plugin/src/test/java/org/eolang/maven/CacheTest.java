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
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test for {@link Cache}.
 * @since 0.60
 */
@ExtendWith(MktmpResolver.class)
final class CacheTest {

    @Test
    @SuppressWarnings("PMD.UnnecessaryLocalRule")
    void compilesSource(@Mktmp final Path temp) throws Exception {
        final Path base = temp.resolve("cache-folder");
        Files.createDirectories(base);
        final Path source = temp.resolve("source.eo");
        Files.writeString(source, String.format("[] > main%n  (stdout \"Hello, EO!\") > @%n"));
        final Path target = temp.resolve("target.xmir");
        final Path tail = source.getFileName();
        final String content = "compiled";
        new Cache(base, p -> content).apply(source, target, tail);
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
                && Files.exists(base.resolve(String.format("%s.sha256", tail))),
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
            Files.readString(base.resolve(String.format("%s.sha256", tail)), encoding),
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
                cache.resolve(String.format("%s.sha256", tail)),
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
                cache.resolve(String.format("%s.sha256", tail)),
                StandardCharsets.UTF_8
            ),
            Matchers.equalTo(CacheTest.hash(content))
        );
    }

    @Test
    @SuppressWarnings("PMD.UnnecessaryLocalRule")
    void generatesCorrectHashForEntireFolderWithSeveralFiles(
        @Mktmp final Path temp
    ) throws IOException, NoSuchAlgorithmException {
        final Path cache = temp.resolve("cache");
        Files.createDirectories(cache);
        final Path source = temp.resolve("folder");
        Files.createDirectories(source);
        final String first = "file1 content";
        final String second = "file2 content";
        final String result = String.format("%s%s", first, second);
        Files.writeString(source.resolve("file1.txt"), first, StandardCharsets.UTF_8);
        Files.writeString(source.resolve("file2.txt"), second, StandardCharsets.UTF_8);
        new Cache(cache, p -> result).apply(
            source,
            temp.resolve("out.txt"),
            source.getFileName()
        );
        final MessageDigest instance = MessageDigest.getInstance("SHA-256");
        instance.update(
            String.format("file1.txt\u0000%s", CacheTest.hash(first)).getBytes(StandardCharsets.UTF_8)
        );
        instance.update(
            String.format("file2.txt\u0000%s", CacheTest.hash(second)).getBytes(StandardCharsets.UTF_8)
        );
        MatcherAssert.assertThat(
            "SHA-256 hash file has incorrect content for folder with several files",
            Files.readString(
                cache.resolve("folder.sha256"),
                StandardCharsets.UTF_8
            ),
            Matchers.equalTo(Base64.getEncoder().encodeToString(instance.digest()))
        );
    }

    @Test
    @SuppressWarnings("PMD.UnnecessaryLocalRule")
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
            "Directories with identically-content but differently-named files "
                + "must produce different hashes",
            Files.readString(cache.resolve("dirA.sha256"), StandardCharsets.UTF_8),
            Matchers.not(
                Matchers.equalTo(
                    Files.readString(cache.resolve("dirB.sha256"), StandardCharsets.UTF_8)
                )
            )
        );
    }

    private static String hash(final String content) throws NoSuchAlgorithmException {
        return Base64.getEncoder().encodeToString(
            MessageDigest.getInstance("SHA-256")
                .digest(content.getBytes(StandardCharsets.UTF_8))
        );
    }
}
