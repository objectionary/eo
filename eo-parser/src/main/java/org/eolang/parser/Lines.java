/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import java.util.List;
import org.cactoos.Text;
import org.cactoos.text.UncheckedText;

/**
 * Addresses a source's lines by number.
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
     *
     * <p>Lines are numbered from 1. A number outside that range is a
     * mistake by the caller, not an empty line, so an
     * {@link IndexOutOfBoundsException} naming the number and the size of
     * the source is thrown, instead of folding it into {@code ""} — which
     * a caller could not tell apart from a real, empty line at a valid
     * number.</p>
     *
     * @param number The line number, from 1 to the number of lines
     * @return The line
     */
    String line(final int number) {
        if (number < 1 || number > this.source.size()) {
            throw new IndexOutOfBoundsException(
                String.format(
                    "Line #%d doesn't exist, the source has %d line(s), numbered from 1",
                    number, this.source.size()
                )
            );
        }
        return new UncheckedText(
            this.source.get(number - 1),
            error -> {
                throw new IllegalStateException(
                    String.format("Failed to read line #%d", number), error
                );
            }
        ).asString();
    }
}
