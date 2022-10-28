package org.eolang.maven;

import java.nio.file.Path;

public class ChFile implements CommitHash {

    private final Path file;
    private final String tag;

    public ChFile(final Path file, final String tag) {
        this.file = file;
        this.tag = tag;
    }

    @Override
    public String value() {


        return null;
    }
}
