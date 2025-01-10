/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2025 Objectionary.com
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

/*
 * @checkstyle PackageNameCheck (10 lines)
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package EOorg.EOeolang.EOfs; // NOPMD

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.BufferedWriter;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.nio.file.Paths;
import org.eolang.ExFailure;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link Files}.
 * @since 0.40
 */
@ExtendWith(MktmpResolver.class)
final class FilesTest {
    @Test
    void throwsOnReadingWithoutOpening(@Mktmp final Path dir) {
        Assertions.assertThrows(
            ExFailure.class,
            () -> Files.INSTANCE.read(
                dir.resolve("c.txt").toFile().getAbsolutePath(), 10
            ),
            "File should not allow to read before opening"
        );
    }

    @Test
    void throwsOnWritingWithoutOpening(@Mktmp final Path dir) {
        Assertions.assertThrows(
            ExFailure.class,
            () -> Files.INSTANCE.write(
                dir.resolve("b.txt").toFile().getAbsolutePath(),
                new byte[]{0x01}
            ),
            "File should not allow to write before opening"
        );
    }

    @Test
    void throwsOnClosingWithoutOpening(@Mktmp final Path dir) {
        Assertions.assertThrows(
            ExFailure.class,
            () -> Files.INSTANCE.close(
                dir.resolve("a.txt").toFile().getAbsolutePath()
            ),
            "File should not allow to close before opening"
        );
    }

    @Test
    void readsFromFile(@Mktmp final Path dir) throws IOException {
        final String file = dir.resolve("bar.txt").toFile().getAbsolutePath();
        try (BufferedWriter writer =
            java.nio.file.Files.newBufferedWriter(Paths.get(file))) {
            writer.write("Hello, world");
        }
        Files.INSTANCE.open(file);
        MatcherAssert.assertThat(
            "The string should have been read from file",
            Files.INSTANCE.read(file, 12),
            Matchers.equalTo("Hello, world".getBytes(StandardCharsets.UTF_8))
        );
        Files.INSTANCE.close(file);
    }

    @Test
    void writesToFile(@Mktmp final Path dir) throws IOException {
        final String file = dir.resolve("foo.txt").toFile().getAbsolutePath();
        try (BufferedWriter writer =
            java.nio.file.Files.newBufferedWriter(Paths.get(file))) {
            writer.write("Hello, world");
        }
        Files.INSTANCE.open(file);
        Files.INSTANCE.write(file, "!".getBytes(StandardCharsets.UTF_8));
        MatcherAssert.assertThat(
            "The string should have been read from file",
            Files.INSTANCE.read(file, 13),
            Matchers.equalTo("Hello, world!".getBytes(StandardCharsets.UTF_8))
        );
        Files.INSTANCE.close(file);
    }
}
