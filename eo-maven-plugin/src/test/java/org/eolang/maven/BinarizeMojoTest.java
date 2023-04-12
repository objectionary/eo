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
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;

/**
 * Test case for {@link BinarizeMojo}.
 *
 * @since 0.1
 */
@Execution(ExecutionMode.CONCURRENT)
final class BinarizeMojoTest {

    /**
     * BinarizeMojo can binarize without errors.
     * @param temp Temporary directory.
     * @throws Exception If fails.
     */
    @Test
    void binarizesWithoutErrors(@TempDir final Path temp) throws Exception {
        final FakeMaven maven;
        synchronized (BinarizeMojoTest.class) {
            maven = new FakeMaven(temp)
                .withProgram(Paths.get("src/test/resources/org/eolang/maven/simple-rust.eo"))
                .withProgram(Paths.get("src/test/resources/org/eolang/maven/twice-rust.eo"));
        }
        Assertions.assertDoesNotThrow(
            () -> maven.execute(new FakeMaven.Binarize())
        );
    }

    @Test
    void failsWithIncorrectInsert(@TempDir final Path temp) throws IOException {
        final Path src = Paths.get("src/test/resources/org/eolang/maven/wrong-rust.eo");
        final FakeMaven maven;
        synchronized (BinarizeMojoTest.class) {
            maven = new FakeMaven(temp)
                .withProgram(src);
        }
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> maven.execute(new FakeMaven.Binarize())
        );
    }
}
