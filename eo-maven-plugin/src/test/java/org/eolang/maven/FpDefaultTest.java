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
}
