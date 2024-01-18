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
package org.eolang.maven.footprint;

import java.io.IOException;
import java.nio.file.Path;
import org.cactoos.Scalar;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

/**
 * Tests for Cached.
 * @since 1.0
 */
final class FtDefaultTest {

    /**
     * Default extension.
     */
    private static final String XMIR = "xmir";

    /**
     * Default content.
     */
    private static final Scalar<String> CONTENT = () -> "content";

    @Test
    void loadsContentOfNoCacheFile(@TempDir final Path temp) throws Exception {
        final String program = "org.eolang.txt";
        final Path target = temp.resolve("target");
        new FtDefault(target).save(program, FtDefaultTest.XMIR, FtDefaultTest.CONTENT);
        MatcherAssert.assertThat(
            new FtDefault(target).load(program, FtDefaultTest.XMIR),
            Matchers.equalTo(FtDefaultTest.CONTENT.value())
        );
    }

    @Test
    void returnsListOfSavedFilesWithoutDirectory(@TempDir final Path temp) throws IOException {
        final Footprint footprint = new FtDefault(temp);
        footprint.save("org.eolang.a", FtDefaultTest.XMIR, FtDefaultTest.CONTENT);
        footprint.save("org.eolang.b", FtDefaultTest.XMIR, FtDefaultTest.CONTENT);
        footprint.save("org.eolang.c", "o", FtDefaultTest.CONTENT);
        footprint.save("org.eolang.dir.sub", "o", FtDefaultTest.CONTENT);
        final Path subfolder = temp.resolve("org").resolve("eolang");
        MatcherAssert.assertThat(
            footprint.list("xmir"),
            Matchers.containsInAnyOrder(
                subfolder.resolve("a.xmir"),
                subfolder.resolve("b.xmir")
            )
        );
        MatcherAssert.assertThat(
            footprint.list("o"),
            Matchers.containsInAnyOrder(
                subfolder.resolve("c.o"),
                subfolder.resolve("dir").resolve("sub.o")
            )
        );
        MatcherAssert.assertThat(
            footprint.list("org"),
            Matchers.empty()
        );
    }
}
