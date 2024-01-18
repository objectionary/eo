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
import org.hamcrest.io.FileMatchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

/**
 * Tests for Cached.
 * @since 1.0
 */
final class FtCachedTest {

    /**
     * Default content.
     */
    private static final Scalar<String> CONTENT = () -> "content";

    /**
     * Default extension.
     */
    private static final String EXTENSION = "xmir";

    /**
     * Default target folder.
     */
    private static final String TARGET = "target";

    /**
     * Default cache folder.
     */
    private static final String CACHE = "parsed";

    @Test
    void loadsContentOfCachedFile(@TempDir final Path temp) throws Exception {
        final Footprint cached = FtCachedTest.footprint("0.29.1", temp);
        final String program = "org.eolang.txt.format";
        cached.save(program, FtCachedTest.EXTENSION, FtCachedTest.CONTENT);
        MatcherAssert.assertThat(
            cached.load(program, FtCachedTest.EXTENSION),
            Matchers.equalTo(FtCachedTest.CONTENT.value())
        );
        MatcherAssert.assertThat(
            temp.resolve(FtCachedTest.CACHE).toFile(),
            FileMatchers.anExistingDirectory()
        );
    }

    @Test
    void loadsContentOfRealFileCachingDoesNotWork(@TempDir final Path temp) throws Exception {
        final Footprint cached = FtCachedTest.footprint("0.0.0", temp);
        final String program = "org.eolang.txt.regex";
        cached.save(program, FtCachedTest.EXTENSION, FtCachedTest.CONTENT);
        MatcherAssert.assertThat(
            cached.load(program, FtCachedTest.EXTENSION),
            Matchers.equalTo(FtCachedTest.CONTENT.value())
        );
        MatcherAssert.assertThat(
            temp.resolve(FtCachedTest.CACHE).toFile(),
            Matchers.not(FileMatchers.anExistingFileOrDirectory())
        );
    }

    @Test
    void returnsListOfSavedFilesFromDelegate(@TempDir final Path temp) throws IOException {
        final Footprint footprint = FtCachedTest.footprint("0.22.1", temp);
        footprint.save("prog", FtCachedTest.EXTENSION, FtCachedTest.CONTENT);
        MatcherAssert.assertThat(
            footprint.list(FtCachedTest.EXTENSION),
            Matchers.hasItem(temp.resolve(FtCachedTest.TARGET).resolve("prog.xmir"))
        );
    }

    private static Footprint footprint(
        final String eover,
        final Path temp
    ) {
        return new FtCached(
            new CacheVersion(eover, "abcde123"),
            temp.resolve(FtCachedTest.CACHE),
            new FtDefault(temp.resolve(FtCachedTest.TARGET))
        );
    }
}
