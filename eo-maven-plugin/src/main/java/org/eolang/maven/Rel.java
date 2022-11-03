package org.eolang.maven;

import java.io.File;
import java.nio.file.Path;
import java.nio.file.Paths;

final class Rel {
    private final Path base;
    private final Path other;

    Rel(final File file) {
        this(file.toPath());
    }

    Rel(final Path other) {
        this(Paths.get(""), other);
    }

    Rel(final Path base, final Path other) {
        this.base = base;
        this.other = other;
    }

    @Override
    public String toString() {
        String path = other.toAbsolutePath().toString();
        if (path.equals(this.base.toString())) {
            path = "./";
        } else if (path.startsWith(this.base.toString())) {
            path = String.format(
                "./%s",
                path.substring(this.base.toString().length() + 1)
            );
        }
        return path;
    }
}
