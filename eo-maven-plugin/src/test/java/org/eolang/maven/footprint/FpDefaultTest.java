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
package org.eolang.maven.footprint;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
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
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test cases for {@link FpDefault}.
 * @since 0.41
 */
@SuppressWarnings("PMD.TooManyMethods")
@ExtendWith(MktmpResolver.class)
final class FpDefaultTest {
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

    /**
     * Snapshot.
     */
    private static final String SNAPSHOT = "SNAPSHOT";

    @Test
    void failsIfSourcePathNotExists(@Mktmp final Path temp) {
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> new FpDefault(
                src -> FpDefaultTest.LAMBDA_CONTENT,
                Paths.get(""),
                "",
                "",
                Paths.get("")
            ).apply(Paths.get(""), Paths.get("")),
            "FpDefault should fail if source path does not exist"
        );
    }

    @Test
    void doesNothingWhenTargetIsOlderThanSource(@Mktmp final Path temp) throws Exception {
        final Path source = FpDefaultTest.existedSource(temp);
        final Path target = FpDefaultTest.existedTarget(temp);
        FpDefaultTest.makeOlder(target);
        final Path result = new FpDefault(
            src -> FpDefaultTest.LAMBDA_CONTENT,
            temp,
            "",
            "",
            Paths.get("")
        ).apply(source, target);
        MatcherAssert.assertThat(
            "Footprint has to return target path, but it didn't",
            result,
            Matchers.equalTo(target)
        );
        MatcherAssert.assertThat(
            "The content of target file has not to be changed",
            new TextOf(target).asString(),
            Matchers.equalTo(FpDefaultTest.TARGET_CONTENT)
        );
    }

    @Test
    void updatesOnlyTargetFromSourceIfNoTargetAndCacheIsNotCacheable(@Mktmp final Path temp)
        throws Exception {
        final Path source = FpDefaultTest.existedSource(temp);
        final Path target = FpDefaultTest.notExistedTarget(temp);
        assert Files.notExists(target);
        new FpDefault(
            src -> FpDefaultTest.LAMBDA_CONTENT,
            temp,
            FpDefaultTest.SNAPSHOT,
            "",
            Paths.get("cache1.txt")
        ).apply(source, target);
        MatcherAssert.assertThat(
            "Target file must be updated from content function, but it didn't",
            new TextOf(target).asString(),
            Matchers.equalTo(FpDefaultTest.LAMBDA_CONTENT)
        );
        MatcherAssert.assertThat(
            "Cache file has not to be updated",
            temp.resolve(FpDefaultTest.SNAPSHOT).resolve("cache.txt").toFile().exists(),
            Matchers.equalTo(false)
        );
    }

    @Test
    void updatesOnlyTargetFromSourceIfYoungerTargetAndCacheIsNotCacheable(@Mktmp final Path temp)
        throws Exception {
        final Path source = FpDefaultTest.existedSource(temp);
        final Path target = FpDefaultTest.existedTarget(temp);
        FpDefaultTest.makeOlder(source);
        new FpDefault(
            src -> FpDefaultTest.LAMBDA_CONTENT,
            temp,
            FpDefaultTest.SNAPSHOT,
            "",
            Paths.get("cache2.txt")
        ).apply(source, target);
        MatcherAssert.assertThat(
            "Target file must be updated from content function, but it didn't",
            new TextOf(target).asString(),
            Matchers.equalTo(FpDefaultTest.LAMBDA_CONTENT)
        );
        MatcherAssert.assertThat(
            "Cache file has not to be updated",
            temp.resolve(FpDefaultTest.SNAPSHOT).resolve("cache.txt").toFile().exists(),
            Matchers.equalTo(false)
        );
    }

    @Test
    void updatesBothIfTargetYoungerAndNotExistedCacheableCache(@Mktmp final Path temp)
        throws Exception {
        final Path source = FpDefaultTest.existedSource(temp);
        final Path target = FpDefaultTest.existedTarget(temp);
        FpDefaultTest.makeOlder(source);
        final Cache cache = FpDefaultTest.notExistedCache(temp);
        assert Files.notExists(cache.path());
        FpDefaultTest.defaultFootprint(cache, source, target);
        MatcherAssert.assertThat(
            "Target content must be updated from lambda, but it didn't",
            new TextOf(target).asString(),
            Matchers.equalTo(FpDefaultTest.LAMBDA_CONTENT)
        );
        MatcherAssert.assertThat(
            "Cache content must be updated from lambda, but it didn't",
            new TextOf(cache.path()).asString(),
            Matchers.equalTo(FpDefaultTest.LAMBDA_CONTENT)
        );
    }

    @Test
    void updatesBothNoTargetAndNotExistedCacheableCache(@Mktmp final Path temp) throws Exception {
        final Path source = FpDefaultTest.existedSource(temp);
        final Path target = FpDefaultTest.notExistedTarget(temp);
        final Cache cache = FpDefaultTest.notExistedCache(temp);
        assert Files.notExists(cache.path());
        FpDefaultTest.defaultFootprint(cache, source, target);
        MatcherAssert.assertThat(
            "Target content must be updated from lambda, but it didn't",
            new TextOf(target).asString(),
            Matchers.equalTo(FpDefaultTest.LAMBDA_CONTENT)
        );
        MatcherAssert.assertThat(
            "Cache content must be updated from lambda, but it didn't",
            new TextOf(cache.path()).asString(),
            Matchers.equalTo(FpDefaultTest.LAMBDA_CONTENT)
        );
    }

    @Test
    void updatesBothIfTargetYoungerAndExistedCacheableCacheIsYounger(@Mktmp final Path temp)
        throws Exception {
        final Path source = FpDefaultTest.existedSource(temp);
        final Path target = FpDefaultTest.existedTarget(temp);
        final Cache cache = FpDefaultTest.existedCache(temp);
        assert Files.exists(cache.path());
        FpDefaultTest.makeOlder(source);
        FpDefaultTest.defaultFootprint(cache, source, target);
        MatcherAssert.assertThat(
            "Target content must be updated from lambda, but it didn't",
            new TextOf(target).asString(),
            Matchers.equalTo(FpDefaultTest.LAMBDA_CONTENT)
        );
        MatcherAssert.assertThat(
            "Cache content must be updated from lambda, but it didn't",
            new TextOf(cache.path()).asString(),
            Matchers.equalTo(FpDefaultTest.LAMBDA_CONTENT)
        );
    }

    @Test
    void updatesBothIfNoTargetAndExistedCacheableCacheIsYounger(@Mktmp final Path temp)
        throws Exception {
        final Path source = FpDefaultTest.existedSource(temp);
        final Path target = FpDefaultTest.notExistedTarget(temp);
        final Cache cache = FpDefaultTest.existedCache(temp);
        assert Files.notExists(target);
        FpDefaultTest.makeOlder(source);
        FpDefaultTest.defaultFootprint(cache, source, target);
        MatcherAssert.assertThat(
            "Target content must be updated from lambda, but it didn't",
            new TextOf(target).asString(),
            Matchers.equalTo(FpDefaultTest.LAMBDA_CONTENT)
        );
        MatcherAssert.assertThat(
            "Cache content must be updated from lambda, but it didn't",
            new TextOf(cache.path()).asString(),
            Matchers.equalTo(FpDefaultTest.LAMBDA_CONTENT)
        );
    }

    @Test
    void copiesFromCacheIfTargetYoungerAndExistedCacheableCacheOlder(@Mktmp final Path temp)
        throws Exception {
        final Path source = FpDefaultTest.existedSource(temp);
        final Path target = FpDefaultTest.existedTarget(temp);
        final Cache cache = FpDefaultTest.existedCache(temp);
        FpDefaultTest.makeOlder(source);
        FpDefaultTest.makeOlder(cache.path(), 100_000);
        FpDefaultTest.defaultFootprint(cache, source, target);
        MatcherAssert.assertThat(
            "Target content must be updated from cache, but it didn't",
            new TextOf(target).asString(),
            Matchers.equalTo(FpDefaultTest.CACHE_CONTENT)
        );
        MatcherAssert.assertThat(
            "Cache content must not be changed, but it did",
            new TextOf(target).asString(),
            Matchers.equalTo(FpDefaultTest.CACHE_CONTENT)
        );
    }

    @Test
    void copiesFromCacheIfNoTaretAndExistedCacheableCacheOlder(@Mktmp final Path temp)
        throws Exception {
        final Path source = FpDefaultTest.existedSource(temp);
        final Path target = FpDefaultTest.notExistedTarget(temp);
        assert Files.notExists(target);
        final Cache cache = FpDefaultTest.existedCache(temp);
        FpDefaultTest.makeOlder(source);
        FpDefaultTest.makeOlder(cache.path(), 100_000);
        FpDefaultTest.defaultFootprint(cache, source, target);
        MatcherAssert.assertThat(
            "Target content must be updated from cache, but it didn't",
            new TextOf(target).asString(),
            Matchers.equalTo(FpDefaultTest.CACHE_CONTENT)
        );
        MatcherAssert.assertThat(
            "Cache content must not be changed, but it did",
            new TextOf(target).asString(),
            Matchers.equalTo(FpDefaultTest.CACHE_CONTENT)
        );
    }

    /**
     * Apply default footprint.
     * @param cache Cache
     * @param source Source
     * @param target Target
     * @throws Exception If fails
     */
    private static void defaultFootprint(final Cache cache, final Path source, final Path target)
        throws Exception {
        new FpDefault(
            src -> FpDefaultTest.LAMBDA_CONTENT,
            cache.base,
            cache.semver,
            cache.hash,
            cache.tail
        ).apply(source, target);
    }

    /**
     * Existed file with content.
     * @param path Path to file
     * @param content Content to insert
     * @return Path to file
     * @throws IOException If failed to store content
     */
    private static Path existedFile(final Path path, final String content) throws IOException {
        return new Saved(content, path).value();
    }

    /**
     * Existed source file with content.
     * @param temp Temporary directory
     * @return Path to source file
     * @throws IOException If failed to store content
     */
    private static Path existedSource(final Path temp) throws IOException {
        final Path source = FpDefaultTest.notExistedSource(temp);
        return FpDefaultTest.existedFile(source, "Source");
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
        final Path source = FpDefaultTest.notExistedTarget(temp);
        return FpDefaultTest.existedFile(source, FpDefaultTest.TARGET_CONTENT);
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
        FpDefaultTest.makeOlder(file, 50_000);
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
     * @return Existed cache with content
     * @throws IOException If failed to store a content to cache
     */
    private static Cache existedCache(final Path temp) throws IOException {
        final Cache cache = FpDefaultTest.notExistedCache(temp);
        FpDefaultTest.existedFile(cache.path(), FpDefaultTest.CACHE_CONTENT);
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
            ver,
            hash,
            Paths.get("")
        );
    }

    /**
     * Not existed global cache.
     * @param temp Temporary directory
     * @return Existed cache with content
     */
    private static Cache notExistedCache(final Path temp) {
        return FpDefaultTest.notExistedCache(temp, "1.2.3", "abcdef");
    }

    /**
     * Cache path.
     * @since 0.41
     * @checkstyle VisibilityModifierCheck (100 lines)
     * @checkstyle ParameterNumberCheck (100 lines)
     */
    private static class Cache {
        /**
         * Base.
         */
        public final Path base;

        /**
         * Semver.
         */
        public final String semver;

        /**
         * Hash.
         */
        public final String hash;

        /**
         * Tail.
         */
        public final Path tail;

        /**
         * Ctor.
         * @param base Base
         * @param semver Semver
         * @param hash Hash
         * @param tail Tail
         */
        Cache(final Path base, final String semver, final String hash, final Path tail) {
            this.base = base;
            this.semver = semver;
            this.hash = hash;
            this.tail = tail;
        }

        public Path path() {
            return this.base.resolve(this.semver).resolve(this.hash).resolve(this.tail);
        }
    }
}
