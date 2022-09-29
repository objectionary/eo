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

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link LatexMojo}.
 *
 * @since 0.1
 */
final class LatexMojoTest {
    @Test
    void testDirectoryAndFileExistence() throws IOException {
        final Path temp = Files.createTempDirectory("eo");
        final Path target = temp.resolve("target");
        new Moja<>(LatexMojo.class)
            .with("targetDir", target.toFile())
            .execute();
        Assertions.assertTrue(Files.exists(target.resolve("latex/universe.tex")));
    }

    @Test
    void testTranslatedFilesExistence() throws IOException {
        final Path temp = Files.createTempDirectory("eo");
        final Path target = temp.resolve("target");
        final Path source = target.resolve("03-optimize/org/eolang");
        this.addFilesToDir(source);
        new Moja<>(LatexMojo.class)
            .with("targetDir", target.toFile())
            .execute();
        Assertions.assertTrue(Files.exists(target.resolve("latex/main.tex")));
        Assertions.assertTrue(Files.exists(target.resolve("latex/a/main.tex")));
        Assertions.assertTrue(Files.exists(target.resolve("latex/c/d/main.tex")));
        Assertions.assertEquals(4, new Walk(target.resolve("latex")).size());
    }

    /**
     * Create source .xmir files for testing.
     *
     * @param dir Directory for source files.
     * @throws IOException If fails
     */
    void addFilesToDir(final Path dir) throws IOException {
        new File(dir.toString()).mkdirs();
        new File(dir.resolve("a").toString()).mkdirs();
        new File(dir.resolve("c/d").toString()).mkdirs();
        final Path zero = dir.resolve("main.xmir");
        final Path one = dir.resolve("a/main.xmir");
        final Path two = dir.resolve("c/d/main.xmir");
        new File(zero.toString()).createNewFile();
        new File(one.toString()).createNewFile();
        new File(two.toString()).createNewFile();
    }

}
