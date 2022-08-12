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

import java.nio.file.Files;
import java.nio.file.Path;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

/**
 * Test case for {@link PlaceMojo}.
 *
 * @since 0.11
 */
public final class PlaceMojoTest {

    @Test
    public void placesBinaries(@TempDir final Path temp) throws Exception {
        final Path bins = temp.resolve(ResolveMojo.DIR);
        final Path classes = temp.resolve("classes");
        new Save("x1", bins.resolve("foo/hello/0.1/EObar/x.bin")).save();
        new Save("x2", bins.resolve("foo/hello/0.1/org/eolang/f/x.a.class")).save();
        new Save("x3", bins.resolve("foo/hello/0.1/org/eolang/t.txt")).save();
        new Moja<>(PlaceMojo.class)
            .with("targetDir", temp.toFile())
            .with("outputDir", classes.toFile())
            .with("placed", temp.resolve("placed.json").toFile())
            .execute();
        MatcherAssert.assertThat(
            Files.exists(classes.resolve("EObar/x.bin")),
            Matchers.is(true)
        );
        MatcherAssert.assertThat(
            Files.exists(classes.resolve("org/eolang/f/x.a.class")),
            Matchers.is(true)
        );
        MatcherAssert.assertThat(
            Files.exists(classes.resolve("org/eolang/t.txt")),
            Matchers.is(true)
        );
    }

}
