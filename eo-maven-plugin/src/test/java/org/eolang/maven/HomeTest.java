/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2022 Objectionary.com
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
package org.eolang.maven;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.NoSuchFileException;
import java.nio.file.Path;
import java.nio.file.Paths;
import org.cactoos.Bytes;
import org.cactoos.text.Randomized;
import org.cactoos.text.TextOf;
import org.cactoos.text.UncheckedText;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

/**
 * Test for {@link Home}.
 *
 * @since 0.22
 */
final class HomeTest {

    @ValueSource(ints = {0, 100, 1_000, 10_000})
    @ParameterizedTest
    void saves(final int size, @TempDir final Path temp) throws IOException {
        final Path resolve = temp.resolve("1.txt");
        final String content = new UncheckedText(new Randomized(size)).asString();
        new Home().save(content, resolve);
        MatcherAssert.assertThat(
            new UncheckedText(new TextOf(resolve)).asString(),
            Matchers.is(content)
        );
    }

    @Test
    void returnsRelativePathOfCurrentWorkingDirectory(@TempDir final Path temp) {
        MatcherAssert.assertThat(
            new Home(temp.resolve("dir")).rel(temp.resolve("dir/file.txt")),
            Matchers.is("./file.txt")
        );
    }

    @Test
    void existsTest(@TempDir final Path temp) throws IOException {
        new Home().save("any content", temp.resolve("file.txt"));
        MatcherAssert.assertThat(
            new Home().exists(temp.resolve("file.txt")),
            Matchers.is(true)
        );
    }

    @Test
    void existsInDirTest(@TempDir final Path temp) throws IOException {
        new Home(Paths.get("dir")).save("any content", temp.resolve("file.txt"));
        MatcherAssert.assertThat(
            new Home(Paths.get("dir")).exists(temp.resolve("file.txt")),
            Matchers.is(true)
        );
    }

    @Test
    void existsInDirDifferentEncryptionTest(@TempDir final Path temp) throws IOException {
        final String filename = "文件名.txt";
        final byte[] bytes = filename.getBytes(StandardCharsets.UTF_16BE);
        final String decoded = new String(bytes, StandardCharsets.UTF_16BE);
        new Home(Paths.get("directory")).save("any content", temp.resolve(decoded));
        MatcherAssert.assertThat(
            new Home(Paths.get("directory")).exists(temp.resolve(filename)),
            Matchers.is(true)
        );
    }

    @Test
    void existsInDirWithSpecialSymbolsTest(@TempDir final Path temp) throws IOException {
        final String filename = "EOorg/EOeolang/EOmath/EOnan$EOas_int$EO@";
        final byte[] bytes = filename.getBytes("CP1252");
        final String decoded = new String(bytes, "CP1252");
        new Home(Paths.get("directory")).save("any content", temp.resolve(decoded));
        MatcherAssert.assertThat(
            new Home(Paths.get("directory")).exists(temp.resolve(filename)),
            Matchers.is(true)
        );
    }

    @Test
    void loadBytesFromExistingFile(@TempDir final Path temp) throws IOException {
        final Home home = new Home();
        final String filename = "foo";
        final String content = "bar";
        final Path subfolder = temp.resolve("subfolder").resolve(filename);
        home.save(content, subfolder);
        final Bytes bytes = home.load(subfolder);
        final TextOf text = new TextOf(bytes);
        MatcherAssert.assertThat(text, Matchers.equalTo(new TextOf(content)));
    }

    @Test
    void loadFromAbsentFile(@TempDir final Path temp) {
        final Home home = new Home();
        final Path absent = temp.resolve("nonexistent");
        Assertions.assertThrows(NoSuchFileException.class, () -> home.load(absent));
    }
}
