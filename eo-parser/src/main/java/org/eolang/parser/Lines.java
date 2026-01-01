/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import java.util.List;
import java.util.Optional;
import org.cactoos.Text;
import org.cactoos.text.UncheckedText;

/**
 * The source in lines.
 * @since 0.50
 */
final class Lines {

    /**
     * The source.
     */
    private final List<Text> source;

    /**
     * Ctor.
     * @param lines The source in lines
     */
    Lines(final List<Text> lines) {
        this.source = lines;
    }

    /**
     * Get the line by number.
     * @param number The line number.
     * @return The line.
     */
    String line(final int number) {
        final Optional<String> result;
        if (number < 1 || number > this.source.size()) {
            result = Optional.empty();
        } else {
            result = Optional.ofNullable(this.source.get(number - 1))
                .map(UncheckedText::new)
                .map(UncheckedText::asString);
        }
        return result.orElse("");
    }
}
