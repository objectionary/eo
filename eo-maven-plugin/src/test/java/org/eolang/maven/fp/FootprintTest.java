/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2024 Objectionary.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */
package org.eolang.maven.fp;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.attribute.FileTime;
import org.cactoos.text.TextOf;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

/**
 * Test cases for {@link Footprint}.
 * @since 0.41.0
 */
@SuppressWarnings("PMD.TooManyMethods")
final class FootprintTest {
    /**
     * Lambda content.
     */
    private static final String LAMBDA_CONTENT = "Footprint content";

    /**
     * Target content.
     */
    private static final String TARGET_CONTENT = "Target content";

    /**
     * Cache content.
     */
    private static final String CACHE_CONTENT = "Cache content";

    @Test
    void failsIfSourcePathNotExists(@TempDir final Path temp) {
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> new Footprint(
                Paths.get("a/b/c"),
                Paths.get(""),
                FootprintTest.notExistedCache(temp)
            ).apply(() -> FootprintTest.LAMBDA_CONTENT),
            "Footprint should fail if source path does not exist"
        );
    }

    @Test
    void doesNothingWhenTargetIsOlderThanSource(@TempDir final Path temp) throws Exception {
        final Path source = FootprintTest.existedSource(temp);
        final Path target = FootprintTest.existedTarget(temp);
        FootprintTest.makeOlder(target);
        final Path result = new Footprint(
            source,
            target,
            FootprintTest.notExistedCache(temp)
        ).apply(() -> FootprintTest.LAMBDA_CONTENT);
        MatcherAssert.assertThat(
            "Footprint has to return target path, but it didn't",
            result,
            Matchers.equalTo(target)
        );
        MatcherAssert.assertThat(
            "The content of target file has not to be changed",
            new TextOf(target).asString(),
            Matchers.equalTo(FootprintTest.TARGET_CONTENT)
        );
    }

    @Test
    void updatesOnlyTargetFromSourceIfNoTargetAndCacheIsNotCacheable(@TempDir final Path temp)
        throws Exception {
        final Path source = FootprintTest.existedSource(temp);
        final Path target = FootprintTest.notExistedTarget(temp);
        assert Files.notExists(target);
        final Cache cache = FootprintTest.existedCache(temp, "SNAPSHOT", "");
        assert Files.exists(cache.path());
        new Footprint(source, target, cache).apply(() -> FootprintTest.LAMBDA_CONTENT);
        MatcherAssert.assertThat(
            "Target file must be updated from content function, but it didn't",
            new TextOf(target).asString(),
            Matchers.equalTo(FootprintTest.LAMBDA_CONTENT)
        );
        MatcherAssert.assertThat(
            "Cache file has not to be updated",
            new TextOf(cache.path()).asString(),
            Matchers.equalTo(FootprintTest.CACHE_CONTENT)
        );
    }

    @Test
    void updatesOnlyTargetFromSourceIfYoungerTargetAndCacheIsNotCacheable(@TempDir final Path temp)
        throws Exception {
        final Path source = FootprintTest.existedSource(temp);
        final Path target = FootprintTest.existedTarget(temp);
        FootprintTest.makeOlder(source);
        final Cache cache = FootprintTest.existedCache(temp, "SNAPSHOT", "");
        new Footprint(source, target, cache).apply(() -> FootprintTest.LAMBDA_CONTENT);
        MatcherAssert.assertThat(
            "Target file must be updated from content function, but it didn't",
            new TextOf(target).asString(),
            Matchers.equalTo(FootprintTest.LAMBDA_CONTENT)
        );
        MatcherAssert.assertThat(
            "Cache file has not to be updated",
            new TextOf(cache.path()).asString(),
            Matchers.equalTo(FootprintTest.CACHE_CONTENT)
        );
    }

    @Test
    void updatesAllIfTargetYoungerAndNotExistedCacheableCache(@TempDir final Path temp)
        throws Exception {
        final Path source = FootprintTest.existedSource(temp);
        final Path target = FootprintTest.existedTarget(temp);
        FootprintTest.makeOlder(source);
        final Cache cache = FootprintTest.notExistedCache(temp);
        assert Files.notExists(cache.path());
        new Footprint(source, target, cache).apply(() -> FootprintTest.LAMBDA_CONTENT);
        MatcherAssert.assertThat(
            "Target content must be updated from lambda, but it didn't",
            new TextOf(target).asString(),
            Matchers.equalTo(FootprintTest.LAMBDA_CONTENT)
        );
        MatcherAssert.assertThat(
            "Cache content must be updated from lambda, but it didn't",
            new TextOf(cache.path()).asString(),
            Matchers.equalTo(FootprintTest.LAMBDA_CONTENT)
        );
    }

    @Test
    void updatesAllNoTargetAndNotExistedCacheableCache(@TempDir final Path temp) throws Exception {
        final Path source = FootprintTest.existedSource(temp);
        final Path target = FootprintTest.notExistedTarget(temp);
        final Cache cache = FootprintTest.notExistedCache(temp);
        assert Files.notExists(cache.path());
        new Footprint(source, target, cache).apply(() -> FootprintTest.LAMBDA_CONTENT);
        MatcherAssert.assertThat(
            "Target content must be updated from lambda, but it didn't",
            new TextOf(target).asString(),
            Matchers.equalTo(FootprintTest.LAMBDA_CONTENT)
        );
        MatcherAssert.assertThat(
            "Cache content must be updated from lambda, but it didn't",
            new TextOf(cache.path()).asString(),
            Matchers.equalTo(FootprintTest.LAMBDA_CONTENT)
        );
    }

    @Test
    void updatesAllIfTargetYoungerAndExistedCacheableCacheIsYounger(@TempDir final Path temp)
        throws Exception {
        final Path source = FootprintTest.existedSource(temp);
        final Path target = FootprintTest.existedTarget(temp);
        final Cache cache = FootprintTest.existedCache(temp);
        assert Files.exists(cache.path());
        FootprintTest.makeOlder(source);
        new Footprint(source, target, cache).apply(() -> FootprintTest.LAMBDA_CONTENT);
        MatcherAssert.assertThat(
            "Target content must be updated from lambda, but it didn't",
            new TextOf(target).asString(),
            Matchers.equalTo(FootprintTest.LAMBDA_CONTENT)
        );
        MatcherAssert.assertThat(
            "Cache content must be updated from lambda, but it didn't",
            new TextOf(cache.path()).asString(),
            Matchers.equalTo(FootprintTest.LAMBDA_CONTENT)
        );
    }

    @Test
    void updatesAllIfNoTargetAndExistedCacheableCacheIsYounger(@TempDir final Path temp)
        throws Exception {
        final Path source = FootprintTest.existedSource(temp);
        final Path target = FootprintTest.notExistedTarget(temp);
        final Cache cache = FootprintTest.existedCache(temp);
        assert Files.notExists(target);
        FootprintTest.makeOlder(source);
        new Footprint(source, target, cache).apply(() -> FootprintTest.LAMBDA_CONTENT);
        MatcherAssert.assertThat(
            "Target content must be updated from lambda, but it didn't",
            new TextOf(target).asString(),
            Matchers.equalTo(FootprintTest.LAMBDA_CONTENT)
        );
        MatcherAssert.assertThat(
            "Cache content must be updated from lambda, but it didn't",
            new TextOf(cache.path()).asString(),
            Matchers.equalTo(FootprintTest.LAMBDA_CONTENT)
        );
    }

    @Test
    void copiesFromCacheIfTargetYoungerAndExistedCacheableCacheOlder(@TempDir final Path temp)
        throws Exception {
        final Path source = FootprintTest.existedSource(temp);
        final Path target = FootprintTest.existedTarget(temp);
        final Cache cache = FootprintTest.existedCache(temp);
        FootprintTest.makeOlder(source);
        FootprintTest.makeOlder(cache.path(), 100_000);
        new Footprint(source, target, cache).apply(() -> FootprintTest.LAMBDA_CONTENT);
        MatcherAssert.assertThat(
            "Target content must be updated from cache, but it didn't",
            new TextOf(target).asString(),
            Matchers.equalTo(FootprintTest.CACHE_CONTENT)
        );
        MatcherAssert.assertThat(
            "Cache content must not be changed, but it did",
            new TextOf(target).asString(),
            Matchers.equalTo(FootprintTest.CACHE_CONTENT)
        );
    }

    @Test
    void copiesFromCacheIfNoTaretAndExistedCacheableCacheOlder(@TempDir final Path temp)
        throws Exception {
        final Path source = FootprintTest.existedSource(temp);
        final Path target = FootprintTest.notExistedTarget(temp);
        assert Files.notExists(target);
        final Cache cache = FootprintTest.existedCache(temp);
        FootprintTest.makeOlder(source);
        FootprintTest.makeOlder(cache.path(), 100_000);
        new Footprint(source, target, cache).apply(() -> FootprintTest.LAMBDA_CONTENT);
        MatcherAssert.assertThat(
            "Target content must be updated from cache, but it didn't",
            new TextOf(target).asString(),
            Matchers.equalTo(FootprintTest.CACHE_CONTENT)
        );
        MatcherAssert.assertThat(
            "Cache content must not be changed, but it did",
            new TextOf(target).asString(),
            Matchers.equalTo(FootprintTest.CACHE_CONTENT)
        );
    }

    /**
     * Existed file with content.
     * @param path Path to file
     * @param content Content to insert
     * @return Path to file
     * @throws IOException If failed to store content
     */
    private static Path existedFile(final Path path, final String content) throws IOException {
        new Saved.Default(content, path).value();
        return path;
    }

    /**
     * Existed source file with content.
     * @param temp Temporary directory
     * @return Path to source file
     * @throws IOException If failed to store content
     */
    private static Path existedSource(final Path temp) throws IOException {
        final Path source = FootprintTest.notExistedSource(temp);
        return FootprintTest.existedFile(source, "Source");
    }

    /**
     * Not existed source file.
     * @param temp Temporary directory
     * @return Path to source file
     */
    private static Path notExistedSource(final Path temp) {
        return temp.resolve("so/ur/ce.txt");
    }

    /**
     * Existed target file with content.
     * @param temp Temporary directory
     * @return Path to target file
     * @throws IOException If failed to store content
     */
    private static Path existedTarget(final Path temp) throws IOException {
        final Path source = FootprintTest.notExistedTarget(temp);
        return FootprintTest.existedFile(source, FootprintTest.TARGET_CONTENT);
    }

    /**
     * Not existed target file.
     * @param temp Temporary directory
     * @return Path to target file
     */
    private static Path notExistedTarget(final Path temp) {
        return temp.resolve("tar/get.txt");
    }

    /**
     * Make file older.
     * @param file Path to file
     * @throws IOException If failed to make file older
     */
    private static void makeOlder(final Path file) throws IOException {
        FootprintTest.makeOlder(file, 50_000);
    }

    /**
     * Make file older.
     * @param file Path to file
     * @param time Time shift from current time
     * @throws IOException If failed to make file older
     */
    private static void makeOlder(final Path file, final long time) throws IOException {
        Files.setLastModifiedTime(
            file,
            FileTime.fromMillis(System.currentTimeMillis() + time)
        );
    }

    /**
     * Existed global cache with content.
     * @param temp Temporary directory
     * @param ver Cache version
     * @param hash Git hash
     * @return Existed cache with content
     * @throws IOException If failed to store a content to cache
     */
    private static Cache existedCache(final Path temp, final String ver, final String hash)
        throws IOException {
        final Cache cache = FootprintTest.notExistedCache(temp, ver, hash);
        FootprintTest.existedFile(cache.path(), FootprintTest.CACHE_CONTENT);
        return cache;
    }

    /**
     * Existed global cache with content.
     * @param temp Temporary directory
     * @return Existed cache with content
     * @throws IOException If failed to store a content to cache
     */
    private static Cache existedCache(final Path temp) throws IOException {
        final Cache cache = FootprintTest.notExistedCache(temp);
        FootprintTest.existedFile(cache.path(), FootprintTest.CACHE_CONTENT);
        return cache;
    }

    /**
     * Not existed global cache.
     * @param temp Temporary directory
     * @param ver Cache version
     * @param hash Git hash
     * @return Existed cache with content
     */
    private static Cache notExistedCache(final Path temp, final String ver, final String hash) {
        return new Cache(
            temp.resolve("cache"),
            new CacheVersion(ver, hash),
            Paths.get("")
        );
    }

    /**
     * Not existed global cache.
     * @param temp Temporary directory
     * @return Existed cache with content
     */
    private static Cache notExistedCache(final Path temp) {
        return FootprintTest.notExistedCache(temp, "1.2.3", "abcdef");
    }
}
