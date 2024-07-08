package org.eolang.maven;

import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import org.apache.commons.io.FilenameUtils;
import org.eolang.maven.clib.JniStub;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

class CompiledCJniLibTest {
    /**
     * Folder with source C files for tests.
     */
    private static final Path SRC = Paths.get("src/test/resources/org/eolang/maven/c-sources");

    /**
     * Source C file for tests.
     */
    private static final String JNI_STUB_C = "jni-stub.c";

    @Test
    public void compilesCorrectSource(@TempDir final Path temp) {
        Assertions.assertDoesNotThrow(
            () -> new CompiledCJniLib(
                SRC.resolve(JNI_STUB_C),
                temp.resolve(FilenameUtils.removeExtension(JNI_STUB_C))
            ).build(),
            "Exception shouldn't been thrown while compiling native library"
        );
    }

    @Test
    public void loadsCorrectSource(@TempDir final Path temp) throws IOException {
        String target = FilenameUtils.removeExtension(JNI_STUB_C);
        new CompiledCJniLib(SRC.resolve(JNI_STUB_C), temp.resolve(target)).build();
        Assertions.assertDoesNotThrow(
            () -> System.load(temp.resolve(target).toString()),
            "Exception shouldn't been thrown while loading native library"
        );

    }

    @Test
    public void runsCompiledCorrectSource(@TempDir final Path temp) throws IOException {
        int value = 10;
        String target = FilenameUtils.removeExtension(JNI_STUB_C);
        new CompiledCJniLib(SRC.resolve(JNI_STUB_C), temp.resolve(target)).build();
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
}