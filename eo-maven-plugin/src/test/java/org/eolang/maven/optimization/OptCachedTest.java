/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2023 Objectionary.com
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
import java.nio.file.Path;
import java.nio.file.Paths;
import java.security.NoSuchAlgorithmException;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import org.eolang.maven.util.HmBase;
import org.eolang.parser.StHash;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.hamcrest.io.FileMatchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.xembly.Directives;
import org.xembly.ImpossibleModificationException;
import org.xembly.Xembler;

/**
 * Test case for {@link org.eolang.maven.optimization.OptCached}.
 * @since 0.28.12
 */
@SuppressWarnings("PMD.TooManyMethods")
final class OptCachedTest {

    /**
     * Test case for XML program in cache.
     *
     * @param tmp Temp dir
     * @throws IOException if I/O fails
     *  Also, see this <a href="https://github.com/objectionary/eo/issues/2727">issue</a>.
     */
    @Test
    void returnsFromCacheIfXmlAlreadyInCache(@TempDir final Path tmp)
        throws IOException, ImpossibleModificationException, NoSuchAlgorithmException {
        final XML program = OptCachedTest.updatedProgram(
            OptCachedTest.program(ZonedDateTime.now())
        );
        OptCachedTest.save(tmp, program);
        MatcherAssert.assertThat(
            "We expected that the program will be returned from the cache.",
            new OptCached(
                path -> {
                    throw new IllegalStateException("This code shouldn't be executed");
                },
                tmp
            ).apply(program),
            Matchers.equalTo(program)
        );
    }

    @Test
    void returnsFromCacheButTimesSaveAndExecuteDifferent(@TempDir final Path tmp)
        throws IOException, ImpossibleModificationException, NoSuchAlgorithmException {
        final XML program = OptCachedTest.updatedProgram(
            OptCachedTest.program(ZonedDateTime.now())
        );
        OptCachedTest.save(tmp, program);
        MatcherAssert.assertThat(
            "We expected that the not immediately saved program will be returned from the cache.",
            new OptCached(
                path -> {
                    throw new IllegalStateException("This code shouldn't be executed");
                },
                tmp
            ).apply(program),
            Matchers.equalTo(program)
        );
    }

    @Test
    void returnsFromCacheCorrectProgram(@TempDir final Path tmp)
        throws IOException, ImpossibleModificationException, NoSuchAlgorithmException {
        final XML prev = OptCachedTest.updatedProgram(
            OptCachedTest.program(ZonedDateTime.now(), "first program")
        );
        OptCachedTest.save(tmp, prev);
        final XML current = OptCachedTest.updatedProgram(
            OptCachedTest.program(ZonedDateTime.now(), "second program")
        );
        MatcherAssert.assertThat(
            "Expecting current program to be compiled, but prev program was returned from cache.",
            new OptCached(
                path -> current,
                tmp
            ).apply(current),
            Matchers.equalTo(current)
        );
    }

    @Test
    void optimizesIfXmlIsAbsentInCache(@TempDir final Path tmp) {
        final XML program = OptCachedTest.program();
        final Path cache = tmp.resolve("cache");
        MatcherAssert.assertThat(
            "We expect that the program will be created and returned as is (same instance)",
            new OptCached(path -> program, cache).apply(program),
            Matchers.sameInstance(program)
        );
        MatcherAssert.assertThat(
            "We expect that the cache saved the program after the first run",
            cache.resolve("main.xmir").toFile(),
            FileMatchers.anExistingFile()
        );
    }

    @Test
    void optimizesBecauseCacheIsExpired(@TempDir final Path tmp) throws IOException {
        final XML outdated = OptCachedTest.program(ZonedDateTime.now().minusMinutes(1));
        final XML updated = OptCachedTest.program(ZonedDateTime.now());
        OptCachedTest.save(tmp, outdated);
        MatcherAssert.assertThat(
            "We expected that the program will be optimized because the cache is expired",
            new OptCached(path -> updated, tmp).apply(outdated),
            Matchers.equalTo(updated)
        );
    }

    @Test
    void optimizesIfTimeIsNotSet(@TempDir final Path tmp) throws IOException {
        final XML without = OptCachedTest.program();
        final XML with = OptCachedTest.program(ZonedDateTime.now());
        OptCachedTest.save(tmp, without);
        MatcherAssert.assertThat(
            "We expected that the program will be optimized because the cache doesn't have time",
            new OptCached(path -> with, tmp).apply(without),
            Matchers.equalTo(with)
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
     * Generates EO program for tests with specified time.
     * @param time Time.
     * @return XML representation of program.
     */
    private static XML program(final ZonedDateTime time) {
        return OptCachedTest.program(time, "same");
    }

    /**
     * Generates EO program for tests with specified time and context.
     * @param time Time.
     * @param something String.
     * @return XML representation of program.
     */
    private static XML program(final ZonedDateTime time, final String something) {
        return new XMLDocument(
            new Xembler(
                new Directives()
                    .add("program")
                    .attr("name", "main")
                    .attr("time", time.format(DateTimeFormatter.ISO_INSTANT))
                    .add("objects")
                    .attr("something", something)
                    .up()
            ).xmlQuietly()
        );
    }

    /**
     * Adds attribute "hash" in EO program for tests.
     * @param xml XML.
     * @return XML representation of program.
     */
    private static XML updatedProgram(final XML xml)
        throws NoSuchAlgorithmException, ImpossibleModificationException {
        final String hash = new StHash.Hash(xml).getHash();
        return new XMLDocument(
            new Xembler(
                new Directives()
                    .xpath("//program").attr("hash", hash)
            ).apply(xml.node())
        );
    }
}
