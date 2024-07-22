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

class DownloadDepsMojoTest {

    private static final Path OUT = Paths.get("target/classes");

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
            Files.exists(temp.resolve(OUT)),
            String.format("Expected that \"%s\" target directory exists", OUT)
        );
    }

    @Test
    void downloadsCorrectly(@TempDir final Path temp) throws IOException {
        new FakeMaven(temp).execute(DownloadDepsMojo.class);
        for (final String dep: DEPS_NAMES) {
            Assertions.assertTrue(
                Files.exists(temp.resolve(OUT).resolve(dep)),
                String.format(
                    "Expected that \"%s\" dependency was downloaded correctly",
                    temp.resolve(OUT).resolve(dep)
                )
            );
        }
    }
}
