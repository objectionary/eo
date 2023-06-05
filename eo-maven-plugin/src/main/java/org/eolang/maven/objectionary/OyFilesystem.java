package org.eolang.maven.objectionary;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import org.cactoos.Input;
import org.cactoos.io.InputOf;

public class OyFilesystem implements Objectionary {

    private final Path home;

    public OyFilesystem() {
        this(OyFilesystem.defaultHome());
    }

    private OyFilesystem(final Path path) {
        this.home = path;
    }

    @Override
    public Input get(final String name) throws IOException {
        return new InputOf(this.object(name));
    }

    @Override
    public boolean contains(final String name) throws IOException {
        return Files.exists(this.object(name));
    }

    private Path object(final String name) {
        return this.home.resolve(
            String.format(
                "src/main/eo/%s.eo",
                name.replace(".", "/")
            )
        );
    }

    private static Path defaultHome() {
        return Paths.get(
            System.getProperty(
                "runtime.path",
                Paths.get("")
                    .toAbsolutePath()
                    .resolve("eo-runtime")
                    .toString()
            )
        );
    }
}
