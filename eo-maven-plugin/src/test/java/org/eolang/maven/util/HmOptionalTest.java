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
package org.eolang.maven.util;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import org.cactoos.text.Randomized;
import org.cactoos.text.TextOf;
import org.cactoos.text.UncheckedText;
import org.eolang.maven.log.CaptureLogs;
import org.eolang.maven.log.Logs;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

/**
 * Test for {@link HmOptional}.
 *
 * @since 0.35.0
 */
final class HmOptionalTest {

    /**
     * Number of generated characters for testing.
     */
    private int size;

    /**
     * String sample for testing.
     */
    private String sample;

    @BeforeEach
    void setUp() throws Exception {
        this.size = 100;
        this.sample = "file.txt";
    }

    @ParameterizedTest
    @CsvSource({"true", "false"})
    void savesIfFileNotExist(final boolean rewrite, @TempDir final Path dir) throws IOException {
        final HmOptional optional = new HmOptional(new HmBase(dir), rewrite);
        final Path file = Paths.get(this.sample);
        final String content = new UncheckedText(new Randomized(this.size)).asString();
        optional.save(content, file);
        MatcherAssert.assertThat(
            new UncheckedText(new TextOf(dir.resolve(file))).asString(),
            Matchers.is(content)
        );
    }

    @Test
    void savesIfFileExistAndRewriteTrue(@TempDir final Path dir) throws IOException {
        final String first = new UncheckedText(new Randomized(this.size)).asString();
        final Path file = Paths.get(this.sample);
        final HmBase base = new HmBase(dir);
        base.save(first, file);
        final HmOptional optional = new HmOptional(base, true);
        final String second = new UncheckedText(new Randomized(this.size)).asString();
        optional.save(second, file);
        MatcherAssert.assertThat(
            new UncheckedText(new TextOf(dir.resolve(file))).asString(),
            Matchers.not(first)
        );
        MatcherAssert.assertThat(
            new UncheckedText(new TextOf(dir.resolve(file))).asString(),
            Matchers.is(second)
        );
    }

    @Test
    @CaptureLogs
    void savesIfFileExistAndRewriteFalse(
        @TempDir final Path dir,
        final Logs out) throws IOException {
        final String first = new UncheckedText(new Randomized(this.size)).asString();
        final Path file = Paths.get(this.sample);
        final HmBase base = new HmBase(dir);
        base.save(first, file);
        final HmOptional optional = new HmOptional(base, false);
        final String second = new UncheckedText(new Randomized(this.size)).asString();
        optional.save(second, file);
        final Path absolute = dir.resolve(file);
        MatcherAssert.assertThat(
            new UncheckedText(new TextOf(absolute)).asString(),
            Matchers.is(first)
        );
        MatcherAssert.assertThat(
            new UncheckedText(new TextOf(absolute)).asString(),
            Matchers.not(second)
        );
        Assertions.assertTrue(
            out.captured().stream().anyMatch(
                log -> log.contains(
                    String.format("Rewriting of the %s file was skipped", absolute)
                )
            )
        );
    }

    @Test
    void exists(@TempDir final Path dir) throws IOException {
        final Path file = Paths.get(this.sample);
        Files.write(dir.resolve(file), "any content".getBytes());
        MatcherAssert.assertThat(
            new HmOptional(new HmBase(dir), true).exists(file),
            Matchers.is(true)
        );
    }

    @Test
    void absolutes(@TempDir final Path dir) throws IOException {
        final Path file = Paths.get(this.sample);
        final HmBase base = new HmBase(dir);
        base.save("", file);
        final Path absolute = dir.resolve(file);
        Assertions.assertEquals(
            absolute,
            new HmOptional(base, true).absolute(file)
        );
    }

    @ParameterizedTest
    @CsvSource({"file.txt", "a/file.txt", "a/b/file.txt"})
    void checksOnlyRelative(final Path file, @TempDir final Path dir) throws IOException {
        final HmBase base = new HmBase(dir);
        base.save("", file);
        Assertions.assertEquals(
            file,
            new HmOptional(base, true).onlyRelative(file)
        );
    }

    @Test
    void loads(@TempDir final Path dir) throws IOException {
        final HmBase base = new HmBase(dir);
        final Path file = Paths.get(this.sample);
        final String text = "Hello World";
        base.save(text, file);
        Assertions.assertEquals(
            text,
            new TextOf(new HmOptional(base, true).load(file)).toString()
        );
    }
}
