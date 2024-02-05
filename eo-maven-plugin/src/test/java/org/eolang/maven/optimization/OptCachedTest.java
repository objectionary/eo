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
package org.eolang.maven.optimization;

import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.attribute.FileTime;
import org.eolang.maven.util.HmBase;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.hamcrest.io.FileMatchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.xembly.Directives;
import org.xembly.Xembler;

/**
 * Test case for {@link org.eolang.maven.optimization.OptCached}.
 * @since 0.28.12
 */
final class OptCachedTest {

    /**
     * Test case for XML program in cache.
     *
     * @param cache Temp cache dir.
     * @param dir Temp program dir
     * @throws IOException if I/O fails
     * @todo #2790:30min There is repeating logic in tests.
     *  Code logic repeats in {@link OptCacedTest},
     *  {@link OptimizeMojoTest.getAlreadyOptimizedResultsFromCache}
     *  and {@link ShakeMojoTest.getAlreadyShakenResultsFromCache}.
     *  To check whether a program is loaded from the cache,
     *  different tests use the same code
     *  { @code Files.setLastModifiedTime(
     *  res,
     *  FileTime.fromMillis(System.currentTimeMillis() + time)
     *  ); }.
     *  We need to think about how to remove this duplication.
     */
    @Test
    void returnsFromCacheIfXmlAlreadyInCache(@TempDir final Path cache, @TempDir final Path dir)
        throws Exception {
        final XML cached = OptCachedTest.program("cached");
        final XML program = OptCachedTest.program();
        OptCachedTest.save(cache, cached, 5000);
        MatcherAssert.assertThat(
            "We expected that the program will be returned from the cache.",
            new OptCached(
                path -> {
                    throw new IllegalStateException("This code shouldn't be executed");
                },
                cache,
                OptCachedTest.save(dir, program)
            ).apply(program),
            Matchers.equalTo(cached)
        );
    }

    @Test
    void returnsFromCacheButLastModifiedTimesDifferent(
        @TempDir final Path cache,
        @TempDir final Path dir
    )
        throws Exception {
        final XML xml = OptCachedTest.program();
        OptCachedTest.save(cache, xml, 2000);
        MatcherAssert.assertThat(
            "We expected that the program will be returned from the cache.",
            new OptCached(
                path -> {
                    throw new IllegalStateException("This code shouldn't be executed");
                },
                cache,
                OptCachedTest.save(dir, xml)
            ).apply(xml),
            Matchers.equalTo(xml)
        );
    }

    @Test
    void returnsFromCacheCorrectProgram(@TempDir final Path cache, @TempDir final Path dir)
        throws Exception {
        final XML second = OptCachedTest.program("second program");
        OptCachedTest.save(
            cache,
            OptCachedTest.program("first program"),
            -2000
        );
        final Path current = OptCachedTest.save(dir, second);
        MatcherAssert.assertThat(
            "Expecting current program to be compiled, but prev program was returned from cache.",
            new OptCached(path -> second, cache, current)
                .apply(second),
            Matchers.equalTo(second)
        );
    }

    @Test
    void optimizesIfXmlIsAbsentInCache(@TempDir final Path cache, @TempDir final Path dir)
        throws Exception {
        final XML program = OptCachedTest.program();
        MatcherAssert.assertThat(
            "We expect that the program will be created and returned as is (same instance)",
            new OptCached(
                path -> program, cache, OptCachedTest.save(dir, program)
            )
                .apply(program),
            Matchers.equalTo(program)
        );
        MatcherAssert.assertThat(
            "We expect that the cache saved the program after the first run",
            cache.resolve("main.xmir").toFile(),
            FileMatchers.anExistingFile()
        );
    }

    @Test
    void optimizesBecauseCacheIsExpired(
        @TempDir final Path cache,
        @TempDir final Path dir) throws Exception {
        final XML current = OptCachedTest.program("new program");
        final Path program = OptCachedTest.save(dir, current);
        OptCachedTest.save(
            cache,
            OptCachedTest.program("old program"),
            -5000
        );
        MatcherAssert.assertThat(
            "We expected that the program will be optimized because the cache is expired",
            new OptCached(path -> current, cache, program)
                .apply(current),
            Matchers.equalTo(current)
        );
    }

    /**
     * Save XML program with hardcoded name to a temp directory.
     * @param tmp Temporary test directory.
     * @param xml XML program.
     * @return Path to saved program.
     */
    private static Path save(
        final Path tmp,
        final XML xml) throws IOException {
        return OptCachedTest.save(tmp, xml, 0);
    }

    /**
     * Save XML program with hardcoded name to a temp
     * directory with setting last modification time.
     * @param tmp Temporary test directory.
     * @param xml XML program.
     * @param time Time in milliseconds added to the current time.
     * @return Path to saved program.
     */
    private static Path save(
        final Path tmp,
        final XML xml,
        final Integer time) throws IOException {
        final Path path = Paths.get("main.xmir");
        final Path res = tmp.resolve(path);
        new HmBase(tmp).save(xml.toString().getBytes(StandardCharsets.UTF_8), path);
        Files.setLastModifiedTime(
            res,
            FileTime.fromMillis(System.currentTimeMillis() + time)
        );
        return res;
    }

    /**
     * Generates EO program for tests.
     * @return XML representation of program.
     */
    private static XML program() {
        return new XMLDocument(
            new Xembler(
                new Directives()
                    .add("program")
                    .attr("name", "main")
                    .up()
            ).xmlQuietly()
        );
    }

    /**
     * Generates EO program for tests with specified time and context.
     * @param something String.
     * @return XML representation of program.
     */
    private static XML program(final String something) {
        return new XMLDocument(
            new Xembler(
                new Directives()
                    .add("program")
                    .attr("name", "main")
                    .attr("something", something)
                    .up()
            ).xmlQuietly()
        );
    }
}
