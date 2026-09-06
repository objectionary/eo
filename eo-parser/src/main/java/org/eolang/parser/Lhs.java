/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import java.util.List;

/**
 * The left-hand side of an only-phi shape — the {@code lhs} of
 * {@code lhs > [params] > name} (§3.10) and of a parenthesised
 * {@code (lhs > [params])} inline-phi (§3.10.7).
 *
 * <p>That text becomes the {@code φ} slot of the formation, and everything
 * the shape has to settle before emitting it is a question about the text
 * alone: how many objects a compact-tuple {@code *N} marker asks for
 * (R-3.9.1), whether the head is a reversed dispatch, whether that
 * dispatch has a receiver at all, whether it is loaded with horizontal
 * arguments — the shape R-3.10.6 forbids as a φ — and whether the
 * expression is bare, a head with no chain and no horizontal arguments,
 * which is what leaves the φ {@link Openness#OPEN} for a deeper-indent
 * body to fill (§4.5).</p>
 *
 * <p>The head of the text ends at the first space that sits at paren depth
 * 0 and outside any string literal, which is what {@link #space()} finds,
 * the way {@code Eo.topLevelMarker} finds other top-level markers.</p>
 *
 * @since 0.75.0
 */
final class Lhs {

    /**
     * The span of the left-hand side.
     */
    private final Span span;

    /**
     * Ctor.
     * @param source The span of the left-hand side
     */
    Lhs(final Span source) {
        this.span = source;
    }

    int stars() {
        final String body = this.span.body();
        final int space = this.space();
        final int result;
        if (space > 0 && body.charAt(space - 1) != '.'
            && space + 1 < body.length() && body.charAt(space + 1) == '*') {
            result = this.digits(space + 2);
        } else {
            result = -1;
        }
        return result;
    }

    Tokens tokens(final int stars) {
        final String body = this.span.body();
        final String head;
        if (stars < 0) {
            head = body;
        } else {
            head = body.substring(0, this.space());
        }
        final Span inner = new Span(
            " ".repeat(this.span.indent()).concat(head), this.span.line()
        );
        return new Tokens(inner.body(), inner);
    }

    boolean receiverless() {
        return this.reversed() && this.rargs().isEmpty();
    }

    boolean loaded() {
        return this.reversed() && !this.rargs().isEmpty();
    }

    boolean bare(final Tokens tokens, final Value head, final boolean reversed) {
        final boolean chained;
        if (reversed) {
            tokens.consumeDispatch();
            chained = true;
        } else {
            chained = !tokens.readChain().isEmpty();
        }
        final boolean empty = tokens.readArgs().isEmpty();
        if (reversed && !empty) {
            throw new ParseError(
                this.span.line(), this.span.indent(),
                "only-phi formation body cannot be a reversed dispatch with horizontal arguments"
            );
        }
        return empty && (chained || !head.group());
    }

    private boolean reversed() {
        final Tokens tokens = new Tokens(this.span.body(), this.span);
        return tokens.reversedAhead(tokens.readValue());
    }

    private List<Value> rargs() {
        final Tokens tokens = new Tokens(this.span.body(), this.span);
        tokens.readValue();
        tokens.consumeDispatch();
        return tokens.readArgs();
    }

    private int space() {
        final String body = this.span.body();
        int depth = 0;
        int found = -1;
        int idx = 0;
        while (idx < body.length() && found < 0) {
            final char glyph = body.charAt(idx);
            if (glyph == '"') {
                idx = Tokens.closingQuote(body, idx);
            } else if (glyph == '(') {
                depth = depth + 1;
            } else if (glyph == ')') {
                depth = depth - 1;
            } else if (depth == 0 && glyph == ' ') {
                found = idx;
            }
            idx = idx + 1;
        }
        return found;
    }

    private int digits(final int from) {
        final String body = this.span.body();
        long count = 0;
        boolean digits = true;
        for (int idx = from; idx < body.length(); idx = idx + 1) {
            final char glyph = body.charAt(idx);
            if (glyph < '0' || glyph > '9') {
                digits = false;
                break;
            }
            if (idx > from && body.charAt(from) == '0') {
                throw new ParseError(
                    this.span.line(), this.span.indent() + from,
                    "integer literal must not have leading zeros"
                );
            }
            count = count * 10 + glyph - '0';
            if (count > Integer.MAX_VALUE) {
                throw new ParseError(
                    this.span.line(), this.span.indent() + from,
                    "compact tuple count is too large"
                );
            }
        }
        final int result;
        if (digits) {
            result = (int) count;
        } else {
            result = -1;
        }
        return result;
    }
}
