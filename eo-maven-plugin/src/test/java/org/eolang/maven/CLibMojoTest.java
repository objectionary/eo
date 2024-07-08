package org.eolang.maven;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

class CLibMojoTest {
    @Test
    void executesWithoutErrors(@TempDir final Path temp) {
        Assertions.assertDoesNotThrow(
            () -> new FakeMaven(temp).execute(CLibMojo.class),
            "Exception shouldn't been thrown"
        );
    }

    @Test
    void throwsExceptionOnEmptySourceDir(@TempDir final Path temp) throws IOException {
        final Path empty = CLibMojoTest.createEmptyDirectory(temp, "empty");
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> new FakeMaven(temp).with(
                "cEoLibDir",
                new File(empty.toString())
            ).execute(CLibMojo.class),
            "An exception should be thrown due to absence of any files in given directory"
        );
    }

    @Test
    void throwsExceptionOnAppropriateSourceAbsence(@TempDir final Path temp) throws IOException {
        final Path dir = CLibMojoTest.createEmptyDirectory(temp, "empty");
        Files.createFile(dir.resolve("source.java"));
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> new FakeMaven(temp).with(
                "cEoLibDir",
                new File(dir.toString())
            ).execute(CLibMojo.class),
            "An exception should be thrown due to absence of C sources in directory"
        );
    }

    @Test
    void executesWithoutErrorsOnEmptyCSource(@TempDir final Path temp) throws IOException {
        final Path dir = CLibMojoTest.createEmptyDirectory(temp, "empty");
        Files.createFile(dir.resolve("source.c"));
        Assertions.assertDoesNotThrow(
            () -> new FakeMaven(temp).with(
                "cEoLibDir",
                new File(dir.toString())
            ).execute(CLibMojo.class),
            "Exception shouldn't been thrown"
        );
    }

    @Test
    void executesWithoutErrorsOnValidCSource(@TempDir final Path temp) throws IOException {
        final Path dir = CLibMojoTest.createEmptyDirectory(temp, "empty");
        Files.createFile(dir.resolve("source.c"));
        Files.write(dir.resolve("source.c"), "void empty();".getBytes());
        Assertions.assertDoesNotThrow(
            () -> new FakeMaven(temp).with(
                "cEoLibDir",
                new File(dir.toString())
            ).execute(CLibMojo.class),
            "Exception shouldn't been thrown"
        );
    }

    @Test
    void throwsExceptionOnInvalidCSource(@TempDir final Path temp) throws IOException {
        final Path dir = CLibMojoTest.createEmptyDirectory(temp, "empty");
        Files.createFile(dir.resolve("source.c"));
        Files.write(dir.resolve("source.c"), "INVLAID".getBytes());
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> new FakeMaven(temp).with(
                "cEoLibDir",
                new File(dir.toString())
            ).execute(CLibMojo.class),
            "Exception shouldn't been thrown"
        );
    }

    private static Path createEmptyDirectory(Path dir, String name) throws IOException {
        if (!Files.exists(dir) || !Files.isDirectory(dir)) {
            throw new IllegalArgumentException("The specified path is not an existing directory.");
        }
        final Path empty = dir.resolve(name);
        Files.createDirectories(empty);
        return empty;
    }
}
