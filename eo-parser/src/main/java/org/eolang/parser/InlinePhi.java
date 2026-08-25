/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import java.util.List;

/**
 * Helpers for emitting an inline {@code > [args]} only-phi formation
 * from inside a grouped value (§3.12 / §9.4).
 *
 * <p>Pulled out of {@link Emissions} so that file stays a thin facade
 * over the literal-rendering recipes. The inline-phi detection and
 * emission only depend on the group body and the surrounding emit
 * context, so they sit here on their own.</p>
 *
 * @since 0.1
 */
final class InlinePhi {

    /**
     * No instances.
     */
    private InlinePhi() {
    }

    /**
     * Emit a grouped value, either as an inline {@code > [args]}
     * only-phi formation (§3.12) or as an ordinary sub-expression.
     * @param emit Emitter
     * @param name Name attribute (or {@code null})
     * @param value The value
     * @param line Source line
     * @checkstyle ParameterNumberCheck (3 lines)
     */
    static void group(
        final Emit emit, final String name, final Value value, final int line
    ) {
        final String inner = value.raw().substring(1, value.raw().length() - 1);
        final int phi = InlinePhi.topLevelInlinePhi(inner);
        if (phi >= 0) {
            InlinePhi.inlinePhi(emit, name, inner, phi, value.pos() + 1, line);
        } else {
            final Span sub = new Span(
                " ".repeat(value.pos() + 1).concat(inner), line
            );
            Emissions.expression(emit, name, new Tokens(sub.body(), sub), line);
        }
    }

    /*
     * Locate the top-level inline-phi ">" and "[" marker in a group
     * body, respecting parentheses and string quoting.
     */
    private static int topLevelInlinePhi(final String body) {
        int depth = 0;
        int found = -1;
        int idx = 0;
        while (idx < body.length() - 2 && found < 0) {
            final char glyph = body.charAt(idx);
            if (glyph == '"') {
                idx = Tokens.closingQuote(body, idx);
            } else if (glyph == '(') {
                depth = depth + 1;
            } else if (glyph == ')') {
                depth = depth - 1;
            } else if (depth == 0 && glyph == '>'
                && body.charAt(idx + 1) == ' ' && body.charAt(idx + 2) == '[') {
                found = idx;
            }
            idx = idx + 1;
        }
        return found;
    }

    /*
     * Emit an inline only-phi formation of the ">" and "[" shape.
     */
    private static void inlinePhi(
        final Emit emit, final String name, final String inner,
        final int phi, final int column, final int line
    ) {
        final int bracket = phi + 2;
        final int close = inner.indexOf(']', bracket);
        if (close < 0) {
            throw new ParseError(
                line, column + bracket,
                "only-phi parameter list missing closing `]`"
            );
        }
        final String lhs = inner.substring(0, phi).stripTrailing();
        final String params = inner.substring(bracket + 1, close);
        final Suffix suffix = new Suffix(
            inner.substring(close + 1),
            new Span(" ".repeat(column).concat(inner), line),
            column + close + 1
        );
        final String label;
        if (suffix.present()) {
            label = suffix.attribute(line, column);
        } else {
            label = name;
        }
        emit.object(label, null, line, column);
        if (!suffix.handle().isEmpty()) {
            emit.local(suffix.handle());
        }
        if (suffix.constant()) {
            emit.constant();
        }
        final List<String> parts = LnOnlyPhi.inlineParams(
            params, new Span(" ".repeat(column + bracket + 1).concat(params), line), 0
        );
        int pcol = column + bracket + 1;
        for (final String param : parts) {
            emit.voidParam(Emissions.mapVoidParam(param), line, pcol);
            pcol = pcol + param.length() + 1;
        }
        final Span sub = new Span(" ".repeat(column).concat(lhs), line);
        Emissions.expression(emit, "φ", new Tokens(sub.body(), sub), line);
        emit.close();
    }
}
