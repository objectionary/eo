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

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

/**
 * Test case for {@link org.eolang.maven.CLibMojo}.
 *
 * @since 0.38
 */
final class CLibMojoTest {
    /**
     * {@link CLibMojo} mojo parameter that holds path to folder with C library sources.
     */
    private static final String CLIB_PARAMETER = "cLibDir";

    @Test
    void executesWithoutErrors(@TempDir final Path temp) {
        Assertions.assertDoesNotThrow(
            () -> new FakeMaven(temp).execute(CLibMojo.class),
            "Exception shouldn't been thrown"
        );
    }

    @Test
    void throwsExceptionOnEmptySourceDir(@TempDir final Path temp) {
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> new FakeMaven(temp).with(
                CLibMojoTest.CLIB_PARAMETER,
                new File(temp.toString())
            ).execute(CLibMojo.class),
            "An exception should be thrown due to absence of any files in given directory"
        );
    }

    @Test
    void throwsExceptionOnAppropriateSourceAbsence(@TempDir final Path temp) throws IOException {
        final String source = "empty.java";
        new File(temp.toString(), source);
        Files.write(temp.resolve(source), "".getBytes());
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> new FakeMaven(temp).with(
                CLibMojoTest.CLIB_PARAMETER,
                new File(temp.toString())
            ).execute(CLibMojo.class),
            "An exception should be thrown due to absence of C sources in directory"
        );
    }

    @Test
    void executesWithoutErrorsOnEmptyCSource(@TempDir final Path temp) throws IOException {
        final String source = "empty.c";
        new File(temp.toString(), source);
        Files.write(temp.resolve(source), "".getBytes());
        Assertions.assertDoesNotThrow(
            () -> new FakeMaven(temp).with(
                CLibMojoTest.CLIB_PARAMETER,
                new File(temp.toString())
            ).execute(CLibMojo.class),
            "Exception shouldn't been thrown"
        );
    }

    @Test
    void executesWithoutErrorsOnValidCSource(@TempDir final Path temp) throws IOException {
        final String source = "valid.c";
        new File(temp.toString(), source);
        Files.write(temp.resolve(source), "void empty();".getBytes());
        Assertions.assertDoesNotThrow(
            () -> new FakeMaven(temp).with(
                CLibMojoTest.CLIB_PARAMETER,
                new File(temp.toString())
            ).execute(CLibMojo.class),
            "Exception shouldn't been thrown"
        );
    }

    @Test
    void throwsExceptionOnInvalidCSource(@TempDir final Path temp) throws IOException {
        final String source = "invalid.c";
        new File(temp.toString(), source);
        Files.write(temp.resolve(source), "INVLAID".getBytes());
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> new FakeMaven(temp).with(
                CLibMojoTest.CLIB_PARAMETER,
                new File(temp.toString())
            ).execute(CLibMojo.class),
            "Exception shouldn't been thrown"
        );
    }
}
