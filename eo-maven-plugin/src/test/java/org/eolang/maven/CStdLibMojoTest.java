package org.eolang.maven;

import java.io.IOException;
import java.nio.file.Path;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

class CStdLibMojoTest {
    @Test
    void executeWithoutErrors(@TempDir final Path temp) throws IOException {
        Assertions.assertDoesNotThrow(
            () -> new FakeMaven(temp).execute(CStdLibMojo.class),
            "Exception shouldn't been thrown"
        );
    }
}
