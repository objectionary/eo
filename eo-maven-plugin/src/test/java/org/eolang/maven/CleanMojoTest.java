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
package org.eolang.maven;

import com.yegor256.WeAreOnline;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import org.cactoos.set.SetOf;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.io.TempDir;

/**
 * Test case for {@link CleanMojo}.
 *
 * @since 0.28.6
 */
class CleanMojoTest {

    @Test
    void cleansSuccessfully(@TempDir final Path temp) throws IOException {
        final Path dir = Files.createDirectories(temp.resolve("target"));
        final Path out = Files.createDirectories(dir.resolve("child"));
        final Path small = Files.createDirectories(out.resolve("child.eo"));
        final Path file = Files.createTempFile(dir, "some", ".eo");
        if (!small.toFile().exists() || !file.toFile().exists()) {
            throw new IllegalStateException("Files not created.");
        }
        new FakeMaven(temp)
            .with("targetDir", dir.toFile())
            .execute(CleanMojo.class);
        MatcherAssert.assertThat(
            !file.toFile().exists() && !small.toFile().exists(),
            Matchers.is(true)
        );
    }

    @Test
    @ExtendWith(WeAreOnline.class)
    void makesFullCompilingLifecycleSuccessfully(@TempDir final Path temp) throws IOException {
        new FakeMaven(temp)
            .withHelloWorld()
            .with("includeSources", new SetOf<>("**.eo"))
            .with("outputDir", temp.resolve("out").toFile())
            .with("placed", temp.resolve("list").toFile())
            .with("cache", temp.resolve("cache/parsed"))
            .with("skipZeroVersions", true)
            .with("central", Central.EMPTY)
            .with("ignoreTransitive", true)
            .execute(RegisterMojo.class)
            .execute(AssembleMojo.class)
            .execute(CleanMojo.class);
        MatcherAssert.assertThat(
            temp.resolve("target").toFile().exists(),
            Matchers.is(false)
        );
    }
}
