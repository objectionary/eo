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

package org.eolang.maven;

import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import org.eolang.maven.objectionary.Objectionary;
import org.eolang.maven.util.Home;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

/**
 * Test case for skip option in {@link SafeMojo}.
 *
 * @since 0.22
 */
class SkipTest {


    @Test
    void skipsCopyMojo(@TempDir final Path temp) throws IOException {
        final Path classes = temp.resolve("classes");
        this.executeCopyMojo(temp, true);
        final Path out = classes.resolve("EO-SOURCES/foo/main.eo");
        MatcherAssert.assertThat(
            new Home(classes).exists(classes.relativize(out)),
            Matchers.is(false)
        );
    }

    @Test
    void executesCopyMojo(@TempDir final Path temp) throws IOException {
        final Path classes = temp.resolve("classes");
        this.executeCopyMojo(temp, false);
        final Path out = classes.resolve("EO-SOURCES/foo/x/main.eo");
        MatcherAssert.assertThat(
            new Home(classes).exists(classes.relativize(out)),
            Matchers.is(true)
        );
    }

    private void executeCopyMojo(
        @TempDir final Path temp,
        final boolean skip
    ) throws IOException {
        new FakeMaven(temp)
            .withProgram(
                "+rt foo:0.0.0",
                "",
                "",
                "[args] > main",
                "  \"0.0.0\" > @"
            )
            .with("sourcesDir", temp.toFile())
            .with("outputDir", temp.resolve("classes").toFile())
            .with("skip", skip)
            .with("version", "1.1.1")
            .execute(CopyMojo.class);
    }
}
