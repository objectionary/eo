package org.eolang.compiler;

import java.nio.file.Path;

public final class StringOutput implements Output {

    private final StringBuilder output;

    public StringOutput(final StringBuilder output) {
        this.output = output;
    }

    @Override
    public void save(final Path file, final String content) {
        this.output.append(content);
    }

}
