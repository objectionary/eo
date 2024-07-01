package org.eolang.maven;

import java.nio.file.Path;
import java.nio.file.Paths;
import org.apache.commons.io.FilenameUtils;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

class CompiledCJniLibTest {
    /**
     * Source C files for tests.
     */
    private static final Path SRC = Paths.get("src/test/resources/org/eolang/maven/c-sources");

    // TODO: figure out how to get rid of hardcode
    private static final String CORRECT = "correct.c";

    // TODO: to add failed tests

    @Test
    public void compilesCorrectSource(@TempDir final Path temp) {

        Assertions.assertDoesNotThrow(
            () -> new CompiledCJniLib(SRC.resolve(CORRECT), temp),
            "Exception shouldn't been thrown"
        );
    }

    // TODO: fix this test
    @Test
    public void runsCompiledCorrectSource(@TempDir final Path temp) {
        int value = 10;
        String target = FilenameUtils.removeExtension(CORRECT);
        new CompiledCJniLib(SRC.resolve(CORRECT), temp);
        System.load(temp.resolve(target).toString());
        Assertions.assertEquals(
            JniStub.fun(value),
            value,
            ""
        );
    }

    public static class JniStub {
        public static native int fun(int param);
    }
}