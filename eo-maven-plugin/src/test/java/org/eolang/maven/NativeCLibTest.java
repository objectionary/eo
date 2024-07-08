package org.eolang.maven;

import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import org.apache.commons.io.FilenameUtils;
import org.eolang.maven.clib.JniStub;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

class NativeCLibTest {
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
    public void compilesCorrectSource(@TempDir final Path temp) {
        Assertions.assertDoesNotThrow(
            () -> new NativeCLib(
                SRC.resolve(CORRECT_SOURCE),
                temp.resolve(FilenameUtils.removeExtension(CORRECT_SOURCE))
            ).build(),
            "Exception shouldn't been thrown while compiling native library"
        );
    }

    @Test
    public void loadsCorrectSource(@TempDir final Path temp) {
        String target = FilenameUtils.removeExtension(CORRECT_SOURCE);
        new NativeCLib(SRC.resolve(CORRECT_SOURCE), temp.resolve(target)).build();
        Assertions.assertDoesNotThrow(
            () -> System.load(temp.resolve(target).toString()),
            "Exception shouldn't been thrown while loading native library"
        );
    }

    @Test
    public void runsCompiledCorrectSource(@TempDir final Path temp) {
        int value = 10;
        String target = FilenameUtils.removeExtension(CORRECT_SOURCE);
        new NativeCLib(SRC.resolve(CORRECT_SOURCE), temp.resolve(target)).build();
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
    public void failsOnSourceCompilationError(@TempDir final Path temp) {
        Assertions.assertThrows(
            IllegalArgumentException.class,
            () -> new NativeCLib(
                SRC.resolve(COMPILE_ERROR),
                temp.resolve(FilenameUtils.removeExtension(COMPILE_ERROR))
            ).build(),
            "Exception shouldn't been thrown while compiling native library"
        );
    }
}