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

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collection;
import org.cactoos.list.ListOf;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

/**
 * Test case for {@link DownloadDepsMojo}.
 *
 * @since 0.39
 */
final class DownloadDepsMojoTest {

    /**
     * Path to result output directory.
     */
    private static final Path OUT = Paths.get("target/classes");

    /**
     * Names of all necessary dependencies.
     */
    private static final Collection<String> DEPS_NAMES = new ListOf<>("jna-5.14.0.jar");

    @Test
    void executesWithoutErrors(@TempDir final Path temp) {
        Assertions.assertDoesNotThrow(
            () -> new FakeMaven(temp).execute(DownloadDepsMojo.class),
            "Exception shouldn't been thrown"
        );
    }

    @Test
    void createsOutDir(@TempDir final Path temp) throws IOException {
        new FakeMaven(temp).execute(DownloadDepsMojo.class);
        Assertions.assertTrue(
            Files.exists(temp.resolve(DownloadDepsMojoTest.OUT)),
            String.format("Expected that \"%s\" target directory exists", DownloadDepsMojoTest.OUT)
        );
    }

    @Test
    void downloadsCorrectly(@TempDir final Path temp) throws IOException {
        new FakeMaven(temp).execute(DownloadDepsMojo.class);
        for (final String dep : DownloadDepsMojoTest.DEPS_NAMES) {
            Assertions.assertTrue(
                Files.exists(temp.resolve(DownloadDepsMojoTest.OUT).resolve(dep)),
                String.format(
                    "Expected that \"%s\" dependency was downloaded correctly",
                    temp.resolve(DownloadDepsMojoTest.OUT).resolve(dep)
                )
            );
        }
    }
}
