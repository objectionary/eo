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
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.NoSuchFileException;
import java.nio.file.Path;
import java.nio.file.Paths;
import org.cactoos.text.Randomized;
import org.cactoos.text.TextOf;
import org.cactoos.text.UncheckedText;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

/**
 * Test for {@link Home}.
 *
 * @since 0.22
 * @todo #1907:30m Enable HomeTest.throwsExceptionOnAbsolute().
 *  The test fails on Windows with no error thrown. It means
 *  that this test didn't throw IllegalArgumentException when we are
 *  trying to check existence of file in temporary directory.
 */
final class HomeTest {

    @ValueSource(ints = {0, 100, 1_000, 10_000})
    @ParameterizedTest
    void saves(final int size, @TempDir final Path temp) throws IOException {
        final Path resolve = Paths.get("1.txt");
        final String content = new UncheckedText(new Randomized(size)).asString();
        new Home(temp).save(content, resolve);
        MatcherAssert.assertThat(
            new UncheckedText(new TextOf(temp.resolve(resolve))).asString(),
            Matchers.is(content)
        );
    }

    @Test
    void exists(@TempDir final Path temp) throws IOException {
        final Path path = Paths.get("file.txt");
        Files.write(temp.resolve(path), "any content".getBytes());
        MatcherAssert.assertThat(
            new Home(temp).exists(path),
            Matchers.is(true)
        );
    }

    @Test
    void existsInDir(@TempDir final Path temp) throws IOException {
        final Path target = temp.resolve("dir/subdir/file.txt");
        target.getParent().toFile().mkdirs();
        Files.write(target, "any content".getBytes());
        MatcherAssert.assertThat(
            new Home(temp.resolve("dir")).exists(Paths.get("subdir/file.txt")),
            Matchers.is(true)
        );
    }

    @Test
    void existsInDirDifferentEncryption(@TempDir final Path temp) throws IOException {
        final String filename = "文件名.txt";
        final byte[] bytes = filename.getBytes(StandardCharsets.UTF_16BE);
        final String decoded = new String(bytes, StandardCharsets.UTF_16BE);
        final Path directory = temp.resolve("directory");
        new Home(directory).save("any content", Paths.get(decoded));
        MatcherAssert.assertThat(
            new Home(directory).exists(Paths.get(filename)),
            Matchers.is(true)
        );
    }

    @Test
    void existsInDirWithSpecialSymbols(@TempDir final Path temp) throws IOException {
        final String filename = "EOorg/EOeolang/EOmath/EOnan$EOas_int$EO@";
        final byte[] bytes = filename.getBytes("CP1252");
        final String decoded = new String(bytes, "CP1252");
        final Path directory = temp.resolve("directory");
        new Home(directory).save("any content", Paths.get(decoded));
        MatcherAssert.assertThat(
            new Home(directory).exists(Paths.get(filename)),
            Matchers.is(true)
        );
    }

    @Test
    void loadsBytesFromExistingFile(@TempDir final Path temp) throws IOException {
        final Home home = new Home(temp);
        final String content = "bar";
        final Path subfolder = Paths.get("subfolder", "foo.txt");
        home.save(content, subfolder);
        MatcherAssert.assertThat(
            new TextOf(home.load(subfolder)),
            Matchers.equalTo(new TextOf(content))
        );
    }

    @Test
    void loadsFromAbsentFile(@TempDir final Path temp) {
        Assertions.assertThrows(
            NoSuchFileException.class,
            () -> new Home(temp).load(Paths.get("nonexistent"))
        );
    }

    @Test
    @Disabled
    void throwsExceptionOnAbsolute(@TempDir final Path temp) {
        Assertions.assertThrows(
            IllegalArgumentException.class,
            () -> new Home(temp).exists(Paths.get("/tmp/file.txt"))
        );
    }
}
