package org.eolang.maven.name;

import java.util.Optional;
import java.util.regex.Pattern;
import org.cactoos.Scalar;
import org.cactoos.scalar.Sticky;
import org.cactoos.scalar.Unchecked;

public final class DelimitedName {

    private static final String DELIMITER = "|";

    private static final Pattern DELIMITER_REGEX = Pattern.compile("\\" + DELIMITER);

    private final Unchecked<String[]> pair;

    DelimitedName(final String raw) {
        this(() -> raw);
    }

    public DelimitedName(final Scalar<String> raw) {
        this(
            new Unchecked<>(
                new Sticky<>(
                    () -> DELIMITER_REGEX.split(raw.value(), 2)
                )
            )
        );
    }

    public DelimitedName(final String title, final Optional<String> label) {
        this(
            new Unchecked<>(
                new Sticky<>(
                    () -> label.map(
                            labelee -> new String[]{title, labelee}
                        )
                        .orElseGet(
                            () -> new String[]{title}
                        )
                )
            )
        );
    }

    private DelimitedName(Unchecked<String[]> pair) {
        this.pair = pair;
    }

    public String title() {
        return this.pair.value()[0];
    }

    public Optional<String> label() {
        String[] pair = this.pair.value();
        return pair.length > 1 ? Optional.of(pair[1]) : Optional.empty();
    }

    @Override
    public String toString() {
        return String.join(DELIMITER, pair.value());
    }
}
