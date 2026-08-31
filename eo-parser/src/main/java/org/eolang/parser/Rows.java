/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import java.util.ArrayList;
import java.util.List;

/**
 * Raw source text addressed by row number, which points at the place where
 * a parse error was found by quoting the offending line under the message.
 *
 * <p>The rows are the {@link Span} objects {@link Source} already
 * produced for the same text, in source order and numbered from 1, so
 * the span of row N sits at index N-1 and carries that row's text.</p>
 *
 * @since 0.50
 */
final class Rows {

    /**
     * The source.
     */
    private final List<Span> source;

    /**
     * Ctor.
     * @param lines The source in lines
     */
    Rows(final List<Span> lines) {
        this.source = new ArrayList<>(lines);
    }

    /**
     * The message with the offending line and a caret beneath it.
     * @param number The line number, 1-indexed
     * @param pos The position in the line, 0-indexed
     * @param message The message
     * @return The message, quoted when the line is known
     */
    String underlined(final int number, final int pos, final String message) {
        final String located = new MsgLocated(number, pos, message).formatted();
        final String result;
        if (number < 1 || number > this.source.size()) {
            result = located;
        } else {
            result = String.format(
                "%s%n%s",
                located,
                new MsgUnderlined(this.source.get(number - 1).text(), pos, 1).formatted()
            );
        }
        return result;
    }
}
