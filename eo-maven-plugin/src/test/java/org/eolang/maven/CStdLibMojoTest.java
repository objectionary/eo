package org.eolang.maven;

import java.nio.file.Path;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

class CStdLibMojoTest {
    @Test
    void executesWithoutErrors(@TempDir final Path temp) {
        Assertions.assertDoesNotThrow(
            () -> new FakeMaven(temp).execute(CStdLibMojo.class),
            "Exception shouldn't been thrown"
        );
    }
}
