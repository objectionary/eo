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
     * @param cache Temp cache dir
     * @param dir Temp program dir
     * @throws IOException if I/O fails
     * @todo #2422:60min returnsFromCacheIfXmlAlreadyInCache: this test is unstable.
     *  We should resolve issues with unstable failures and only
     *  then enable the test.
     *  Also, see this <a href="https://github.com/objectionary/eo/issues/2727">issue</a>.
     */
    @Test
    void returnsFromCacheIfXmlAlreadyInCache(@TempDir final Path cache, @TempDir final Path dir)
        throws IOException {
        final XML xml = OptCachedTest.program();
        final FileTime time = FileTime.fromMillis(System.currentTimeMillis());
        final Path program = OptCachedTest.save(dir, xml);
        OptCachedTest.setTime(dir, time);
        OptCachedTest.save(cache, xml);
        OptCachedTest.setTime(cache, time);
        MatcherAssert.assertThat(
            "We expected that the program will be returned from the cache.",
            new OptCached(
                path -> {
                    throw new IllegalStateException("This code shouldn't be executed");
                },
                cache
            ).apply(program),
            Matchers.equalTo(xml)
        );
    }

    @Test
    void returnsFromCacheButTimesSaveAndExecuteDifferent(
        @TempDir final Path cache,
        @TempDir final Path dir
    )
        throws IOException {
        final XML xml = OptCachedTest.program();
        final Path program = OptCachedTest.save(dir, xml);
        OptCachedTest.setTime(
            dir,
            FileTime.fromMillis(System.currentTimeMillis())
        );
        OptCachedTest.save(cache, xml);
        OptCachedTest.setTime(
            cache,
            FileTime.fromMillis(System.currentTimeMillis() + 2000)
        );
        MatcherAssert.assertThat(
            "We expected that the program will be returned from the cache.",
            new OptCached(
                path -> {
                    throw new IllegalStateException("This code shouldn't be executed");
                },
                cache
            ).apply(program),
            Matchers.equalTo(xml)
        );
    }

    @Test
    void returnsFromCacheCorrectProgram(@TempDir final Path cache, @TempDir final Path dir)
        throws IOException {
        OptCachedTest.save(
            cache,
            OptCachedTest.program("first program")
        );
        final Path current = OptCachedTest.save(
            dir,
            OptCachedTest.program("second program")
        );
        MatcherAssert.assertThat(
            "Expecting current program to be compiled, but prev program was returned from cache.",
            new OptCached(
                path -> OptCachedTest.program("second program"),
                cache
            ).apply(current),
            Matchers.equalTo(OptCachedTest.program("second program"))
        );
    }

    @Test
    void optimizesIfXmlIsAbsentInCache(@TempDir final Path cache, @TempDir final Path dir)
        throws IOException {
        final Path program = OptCachedTest.save(dir, OptCachedTest.program());
        MatcherAssert.assertThat(
            "We expect that the program will be created and returned as is (same instance)",
            new OptCached(path -> OptCachedTest.program(), cache).apply(program),
            Matchers.equalTo(OptCachedTest.program())
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
        @TempDir final Path dir) throws IOException {
        final Path program = OptCachedTest.save(
            dir,
            OptCachedTest.program("new program")
        );
        OptCachedTest.setTime(dir, FileTime.fromMillis(System.currentTimeMillis()));
        OptCachedTest.save(
            cache,
            OptCachedTest.program("old program")
        );
        OptCachedTest.setTime(
            cache,
            FileTime.fromMillis(System.currentTimeMillis() - 5000)
        );
        MatcherAssert.assertThat(
            "We expected that the program will be optimized because the cache is expired",
            new OptCached(
                path -> OptCachedTest.program("new program"),
                cache
            )
                .apply(program),
            Matchers.equalTo(OptCachedTest.program("new program"))
        );
    }

    /**
     * Save XML program with hardcoded name to a temp directory.
     * @param tmp Temporary test directory.
     * @param xml XML program.
     * @return Path to saved program.
     */
    private static Path save(final Path tmp, final XML xml) throws IOException {
        final Path path = Paths.get("main.xmir");
        final Path res = tmp.resolve(path);
        new HmBase(tmp).save(xml.toString().getBytes(StandardCharsets.UTF_8), path);
        return res;
    }

    /**
     * Set time of XML program use absolute path of program.
     * @param path Temporary test directory.
     * @param time XML program.
     * @return Path to saved program.
     */
    private static void setTime(
        final Path path,
        final FileTime time) throws IOException {
        final Path program = path.resolve(Paths.get("main.xmir"));
        Files.setLastModifiedTime(program, time);
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
