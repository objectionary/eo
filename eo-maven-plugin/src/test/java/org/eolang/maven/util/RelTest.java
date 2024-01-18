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
package org.eolang.maven.util;

import java.nio.file.Path;
import java.nio.file.Paths;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

/**
 * Test case for {@link Rel}.
 *
 * @since 0.28.11
 */
class RelTest {

    @ParameterizedTest
    @CsvSource({
        "'file.txt', './file.txt'",
        "'dir/file.txt', './dir/file.txt'",
        "'long/path/to/file.txt', './long/path/to/file.txt'",
        "'../file.txt', './../file.txt'",
        "'./file.txt', '././file.txt'"
    })
    void returnsRelativePathOfCurrentWorkingDirectory(
        final String file,
        final String expected,
        @TempDir final Path temp
    ) {
        MatcherAssert.assertThat(
            new Rel(temp, temp.resolve(file)).toString(),
            Matchers.is(Paths.get(expected).toString())
        );
    }

    @Test
    void returnsAbsolutePathIfBaseAndOtherFromDifferentHierarchies() {
        MatcherAssert.assertThat(
            new Rel(
                Paths.get("/a/b/c"),
                Paths.get("/d/e/f")
            ).toString(),
            Matchers.is(Paths.get("/d/e/f").toAbsolutePath().toString())
        );
    }
}
