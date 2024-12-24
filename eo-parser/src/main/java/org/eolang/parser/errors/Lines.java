package org.eolang.parser.errors;

import java.util.List;
import java.util.Optional;
import org.cactoos.Text;
import org.cactoos.text.UncheckedText;

final class Lines {

    /**
     * The source.
     */
    private final List<Text> lines;

    Lines(final List<Text> lines) {
        this.lines = lines;
    }

    /**
     * Get the line by number.
     * @param number The line number.
     * @return The line.
     */
    Optional<String> line(final int number) {
        final Optional<String> result;
        if (number < 1 || number > this.lines.size()) {
            result = Optional.empty();
        } else {
            result = Optional.ofNullable(this.lines.get(number - 1))
                .map(UncheckedText::new)
                .map(UncheckedText::asString);
        }
        return result;
    }
}
