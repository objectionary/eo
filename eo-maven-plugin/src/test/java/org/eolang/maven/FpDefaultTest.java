/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.attribute.FileTime;
import org.cactoos.text.TextOf;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test cases for {@link FpDefault}.
 * @since 0.41
 */
@SuppressWarnings("PMD.TooManyMethods")
@ExtendWith(MktmpResolver.class)
final class FpDefaultTest {

    @Test
    void failsIfSourcePathNotExists() {
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> new FpDefault(
                src -> FpDefaultTest.footprintContent(),
                Paths.get("/file-doesnt-exist"),
                "",
                "",
                Paths.get("/also-doesnt-exist")
            ).apply(Paths.get("/is-absent"), Paths.get("/not-found")),
            "FpDefault should fail if source path does not exist"
        );
    }

    @Test
    void copiesFromCacheIfNoTaretAndExistedCacheableCacheOlder(
        @Mktmp final Path temp
    ) throws Exception {
        final Path source = FpDefaultTest.existedSource(temp);
        final Path target = FpDefaultTest.notExistedTarget(temp);
        final Cache cache = FpDefaultTest.existedCache(temp);
        FpDefaultTest.makeOlder(source);
        FpDefaultTest.makeOlder(cache.path(), 100_000);
        FpDefaultTest.defaultFootprint(cache, source, target);
        MatcherAssert.assertThat(
            "Target content must be updated from cache, but it didn't",
            new TextOf(target).asString(),
            Matchers.equalTo(FpDefaultTest.cacheContent())
        );
        MatcherAssert.assertThat(
            "Cache content must not be changed, but it did",
            new TextOf(cache.path()).asString(),
            Matchers.equalTo(FpDefaultTest.cacheContent())
        );
    }

    @Test
    void skipsCacheIfItIsNotEnabled(@Mktmp final Path temp) throws Exception {
        final Path source = FpDefaultTest.existedSource(temp);
        final Path target = FpDefaultTest.notExistedTarget(temp);
        final Cache cache = FpDefaultTest.existedCache(temp);
        FpDefaultTest.makeOlder(source);
        FpDefaultTest.makeOlder(cache.path(), 100_000);
        new FpDefault(
            src -> FpDefaultTest.footprintContent(),
            cache.base,
            cache.semver,
            () -> cache.hash,
            cache.tail,
            false
        ).apply(source, target);
        MatcherAssert.assertThat(
            "Global cache must be skipped because of the boolean flag",
            new TextOf(target).asString(),
            Matchers.allOf(
                Matchers.equalTo(FpDefaultTest.footprintContent()),
                Matchers.not(Matchers.equalTo(FpDefaultTest.cacheContent()))
            )
        );
    }

    @Test
    void throwsNpeIfHashIsNull(@Mktmp final Path temp) throws IOException {
        final Path source = FpDefaultTest.existedSource(temp);
        final Path target = FpDefaultTest.notExistedTarget(temp);
        final Cache cache = FpDefaultTest.existedCache(temp);
        Assertions.assertThrows(
            NullPointerException.class,
            () -> new FpDefault(
                src1 -> FpDefaultTest.footprintContent(),
                cache.base,
                cache.semver,
                null,
                cache.tail
            ).apply(source, target),
            "Should throw NPE if hash is null"
        );
    }

    @Test
    void usesCacheEvenItIsSnapshot(@Mktmp final Path temp) throws Exception {
        final Path source = FpDefaultTest.existedSource(temp);
        final Path target = FpDefaultTest.notExistedTarget(temp);
        final String version = "1.0-SNAPSHOT";
        final String hash = "snapshothash";
        final Cache cache = FpDefaultTest.notExistedCache(temp, version, hash);
        FpDefaultTest.existedFile(cache.path(), FpDefaultTest.cacheContent());
        FpDefaultTest.makeOlder(cache.path());
        new FpDefault(
            src -> FpDefaultTest.footprintContent(),
            temp.resolve("cache"),
            version,
            hash,
            Path.of("")
        ).apply(source, target);
        MatcherAssert.assertThat(
            "We expect that cache is used even for SNAPSHOT version",
            new TextOf(target).asString(),
            Matchers.allOf(
                Matchers.equalTo(FpDefaultTest.cacheContent()),
                Matchers.not(Matchers.equalTo(FpDefaultTest.footprintContent()))
            )
        );
    }

    @Test
    void cachesEvenItIsZeroVersion(@Mktmp final Path temp) throws Exception {
        final Path source = FpDefaultTest.existedSource(temp);
        final Path target = FpDefaultTest.notExistedTarget(temp);
        final String version = "0.0.0";
        final String hash = "zerohash";
        final Cache cache = FpDefaultTest.notExistedCache(temp, version, hash);
        FpDefaultTest.existedFile(cache.path(), FpDefaultTest.cacheContent());
        FpDefaultTest.makeOlder(cache.path());
        new FpDefault(
            src -> FpDefaultTest.footprintContent(),
            temp.resolve("cache"),
            version,
            hash,
            Path.of("")
        ).apply(source, target);
        MatcherAssert.assertThat(
            "We expect that cache is used even for 0.0.0 version",
            new TextOf(target).asString(),
            Matchers.allOf(
                Matchers.equalTo(FpDefaultTest.cacheContent()),
                Matchers.not(Matchers.equalTo(FpDefaultTest.footprintContent()))
            )
        );
    }

    /**
     * This test was added to mitigate an issue with the transpilation cache.
     * You can read more about it here:
     * <a href="https://github.com/objectionary/eo/issues/4840">4840</a>
     * If cache is missing, then transpilation should be performed.
     * @param temp Temporary directory
     * @throws Exception If fails
     */
    @Test
    void transpilesIfCacheMiss(@Mktmp final Path temp) throws Exception {
        final Path cdir = temp.resolve("cache-folder").resolve("transpiled-folder");
        Files.createDirectories(cdir);
        final String content = "Source content - no cache";
        final Path source = FpDefaultTest.existedFile(
            temp.resolve("target/eo/1-parse/org/eolang/examples/fibonacci.xmir"),
            "old"
        );
        final Path target = FpDefaultTest.notExistedTarget(
            temp.resolve("target/eo/5-transpile/org/eolang/examples/fibonacci.xmir")
        );
        new FpDefault(
            src -> content,
            cdir,
            "1.0-SNAPSHOT",
            () -> "94641989dbaebef167cf67906a265d925bbb4e5b",
            Paths.get("org/eolang/examples/fibonacci.xmir"),
            true
        ).apply(source, target);
        MatcherAssert.assertThat(
            "We expect a cache miss to trigger transpilation",
            new TextOf(target).asString(),
            Matchers.equalTo(content)
        );
    }

    /**
     * We should use the transpilation cache if it existed before transpilation.
     * Current implementation of {@link FpDefault} relies on file timestamps to decide
     * whether to use the cache or not.
     * If the cache file is older than the source file, then the cache is ignored, which is
     * a critical issue for the transpilation step, because the source files are often
     * modified (e.g. by the parser) right before transpilation, making the cache
     * always older than the source files.
     * @param temp Temporary directory
     * @throws Exception If fails
     * @todo #4840:90min Implement proper cache validation mechanism.
     *  Currently, FpDefault relies on file timestamps to decide whether to use
     *  the cache or not. This approach is not reliable in many cases.
     *  For example, in the transpilation step, the source files are often
     *  modified right before transpilation, making the cache always older
     *  than the source files.
     */
    @Test
    @Disabled
    void doesNotTranspileIfCacheHitWhenCacheExistedBefore(@Mktmp final Path temp) throws Exception {
        final Path cdir = temp.resolve("cache-dir").resolve("transpiled-cache");
        Files.createDirectories(cdir);
        final Path cachefile = cdir
            .resolve("1.0-SNAPSHOT")
            .resolve("94641989dbaebef167cf67906a265d925bbb4e5b")
            .resolve("org/eolang/examples/fibonacci.xmir");
        Files.createDirectories(cachefile.getParent());
        final String cached = "transpilation results from cache";
        Files.write(cachefile, cached.getBytes(StandardCharsets.UTF_8));
        FpDefaultTest.makeOlder(cdir, 1000);
        final Path source = FpDefaultTest.existedFile(
            temp.resolve("target/eo/1-parse/org/eolang/examples/fibonacci.xmir"),
            "Source content"
        );
        final Path target = temp.resolve(
            "target/eo/5-transpile/org/eolang/examples/fibonacci.xmir"
        );
        new FpDefault(
            src -> "transpiled content",
            cdir,
            "1.0-SNAPSHOT",
            () -> "94641989dbaebef167cf67906a265d925bbb4e5b",
            Paths.get("org/eolang/examples/fibonacci.xmir"),
            true
        ).apply(source, target);
        MatcherAssert.assertThat(
            "We expect that cache is used",
            new TextOf(target).asString(),
            Matchers.equalTo(cached)
        );
    }

    /**
     * Returns the cache content.
     */
    private static String cacheContent() {
        return "Cache content";
    }

    /**
     * Returns the footprint content.
     */
    private static String footprintContent() {
        return "Footprint content";
    }

    /**
     * Apply default footprint.
     * @param cache Cache
     * @param source Source
     * @param target Target
     * @throws Exception If fails
     */
    private static void defaultFootprint(
        final Cache cache, final Path source, final Path target
    ) throws Exception {
        new FpDefault(
            src -> FpDefaultTest.footprintContent(),
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
        FpDefaultTest.existedFile(cache.path(), "Cache content");
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
    private static final class Cache {
        /**
         * Base.
         */
        final Path base;

        /**
         * Semver.
         */
        final String semver;

        /**
         * Hash.
         */
        final String hash;

        /**
         * Tail.
         */
        final Path tail;

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

        Path path() {
            return this.base.resolve(this.semver).resolve(this.hash).resolve(this.tail);
        }
    }
}
