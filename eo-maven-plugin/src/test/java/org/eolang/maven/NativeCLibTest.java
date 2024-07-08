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

import java.nio.file.Path;
import java.nio.file.Paths;
import org.apache.commons.io.FilenameUtils;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

/**
 * Test case for {@link org.eolang.maven.NativeCLib}.
 *
 * @since 0.38
 */
final class NativeCLibTest {
    /**
     * Folder with source C files for tests.
     */
    private static final Path SRC = Paths.get("src/test/resources/org/eolang/maven/c-sources");

    /**
     * Correct source C file for tests.
     */
    private static final String CORRECT_SOURCE = "correct.c";

    /**
     * C source for tests with compile error.
     */
    private static final String COMPILE_ERROR = "compile-error.c";

    @Test
    void compilesCorrectSource(@TempDir final Path temp) {
        Assertions.assertDoesNotThrow(
            () -> new NativeCLib(
                NativeCLibTest.SRC.resolve(NativeCLibTest.CORRECT_SOURCE),
                temp.resolve(FilenameUtils.removeExtension(NativeCLibTest.CORRECT_SOURCE))
            ).compile(),
            "Exception shouldn't been thrown while compiling native library"
        );
    }

    @Test
    void loadsCorrectSource(@TempDir final Path temp) {
        final String target = FilenameUtils.removeExtension(NativeCLibTest.CORRECT_SOURCE);
        new NativeCLib(
            NativeCLibTest.SRC.resolve(NativeCLibTest.CORRECT_SOURCE),
            temp.resolve(target)
        ).compile();
        Assertions.assertDoesNotThrow(
            () -> System.load(temp.resolve(target).toString()),
            "Exception shouldn't been thrown while loading native library"
        );
    }

    @Test
    void runsCompiledCorrectSource(@TempDir final Path temp) {
        final int value = 10;
        final String target = FilenameUtils.removeExtension(NativeCLibTest.CORRECT_SOURCE);
        new NativeCLib(
            NativeCLibTest.SRC.resolve(NativeCLibTest.CORRECT_SOURCE),
            temp.resolve(target)
        ).compile();
        System.load(temp.resolve(target).toString());
        Assertions.assertDoesNotThrow(
            () -> JniStub.returnParam(value),
            "The native function should been loaded correctly, but it didn't"
        );
        Assertions.assertEquals(
            JniStub.returnParam(value),
            value,
            "The native function should have returned it's param, but it didn't"
        );
    }

    @Test
    void failsOnSourceCompilationError(@TempDir final Path temp) {
        Assertions.assertThrows(
            IllegalArgumentException.class,
            () -> new NativeCLib(
                NativeCLibTest.SRC.resolve(NativeCLibTest.COMPILE_ERROR),
                temp.resolve(FilenameUtils.removeExtension(NativeCLibTest.COMPILE_ERROR))
            ).compile(),
            "Exception shouldn't been thrown while compiling native library"
        );
    }
}
