package org.eolang.maven;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

class DownloadJnaMojoTest {

    private static final Path OUT = Paths.get("target/classes");

    private static final String JNA_JAR = "jna-5.14.0.jar";

    @Test
    void executesWithoutErrors(@TempDir final Path temp) {
        Assertions.assertDoesNotThrow(
            () -> new FakeMaven(temp).execute(DownloadJnaMojo.class),
            "Exception shouldn't been thrown"
        );
    }

    @Test
    void createsOutDir(@TempDir final Path temp) throws IOException {
        new FakeMaven(temp).execute(DownloadJnaMojo.class);
        Assertions.assertTrue(
            Files.exists(temp.resolve(OUT))
        );
    }

    @Test
    void downloadsCorrectly(@TempDir final Path temp) throws IOException {
        new FakeMaven(temp).execute(DownloadJnaMojo.class);
        Assertions.assertTrue(
            Files.exists(temp.resolve(OUT).resolve(JNA_JAR))
        );
    }
}
