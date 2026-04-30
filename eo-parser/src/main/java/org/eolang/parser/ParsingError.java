/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import java.util.regex.Pattern;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.Token;

/**
 * Parsing error from context.
 * @since 0.56
 */
final class ParsingError extends RuntimeException {

    /**
     * EOL pattern used for splitting input into lines.
     */
    private static final Pattern EOL = Pattern.compile("\\r?\\n");

    /**
     * Ctor.
     * @param cause Underlying parsing exception
     */
    ParsingError(final ParsingException cause) {
        super(cause);
    }

    /**
     * Get cause as {@link ParsingException}.
     * @return Cause
     */
    ParsingException cause() {
        return (ParsingException) this.getCause();
    }

    /**
     * Build a {@link ParsingError} from the given context and message.
     * @param ctx Context
     * @param msg Message
     * @return Parsing error
     */
    static ParsingError fromContext(final ParserRuleContext ctx, final String msg) {
        final int line = ctx.getStart().getLine();
        final int pos = ctx.getStart().getCharPositionInLine();
        return new ParsingError(
            new ParsingException(
                new IllegalStateException("Parsing error"),
                line,
                String.join(
                    String.format("%n"),
                    new MsgLocated(line, pos, msg).formatted(),
                    new MsgUnderlined(
                        ParsingError.line(ctx),
                        pos,
                        ctx.getText().length()
                    ).formatted()
                )
            )
        );
    }

    /**
     * Get line from context.
     * @param ctx Context
     * @return Line
     */
    private static String line(final ParserRuleContext ctx) {
        final Token token = ctx.start;
        final int number = token.getLine();
        final String[] lines = ParsingError.EOL.split(token.getInputStream().toString());
        if (number > 0 && number <= lines.length) {
            return lines[number - 1];
        } else {
            throw new IllegalArgumentException(
                String.format(
                    "Line number '%s' out of bounds, total lines: %d",
                    number,
                    lines.length
                )
            );
        }
    }
}
