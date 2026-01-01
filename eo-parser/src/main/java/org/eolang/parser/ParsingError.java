/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.Token;

/**
 * Parsing error from context.
 *
 * @since 0.56
 */
final class ParsingError extends RuntimeException {
    /**
     * Ctor.
     * @param ctx Context
     * @param msg Message
     */
    ParsingError(final ParserRuleContext ctx, final String msg) {
        super(
            new ParsingException(
                ctx.getStart().getLine(),
                new MsgLocated(
                    ctx.getStart().getLine(),
                    ctx.getStart().getCharPositionInLine(),
                    msg
                ).formatted(),
                new MsgUnderlined(
                    ParsingError.line(ctx),
                    ctx.getStart().getCharPositionInLine(),
                    ctx.getText().length()
                ).formatted()
            )
        );
    }

    /**
     * Get cause as {@link ParsingException}.
     * @return Cause
     */
    ParsingException cause() {
        return (ParsingException) this.getCause();
    }

    /**
     * Get line from context.
     * @param ctx Context
     * @return Line
     */
    private static String line(final ParserRuleContext ctx) {
        final Token token = ctx.start;
        final int number = token.getLine();
        final String[] lines = token.getInputStream().toString().split("\n");
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
