package org.eolang.maven;

import java.nio.file.Path;
import java.util.Arrays;
import org.cactoos.Scalar;
import org.cactoos.io.InputOf;
import org.cactoos.text.TextOf;

final class ChText implements CommitHash {

    private final Scalar<String> source;
    private final String tag;

    ChText(final Path file, final String tag) {
        this(() -> new TextOf(new InputOf(file)).asString(), tag);
    }

    ChText(final Scalar<String> source, final String tag) {
        this.source = source;
        this.tag = tag;
    }

    @Override
    public String value() {
        try {
            return Arrays.stream(source.value().split("\n"))
                .filter(s -> s.contains(tag))
                .findAny()
                .orElseThrow(NotFound::new)
                .split("\\s+")[0];
        } catch (Exception ex) {
            throw new IllegalStateException(
                String.format(
                    "The exception occurred during reading commit hash by tag %s from text source %s",
                    tag,
                    source
                ),
                ex
            );
        }
    }

    private class NotFound extends Exception {
        public NotFound() {
            super(String.format("The hash wasn't found for tag %s", ChText.this.tag));
        }
    }
}
