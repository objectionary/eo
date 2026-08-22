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
 * over the literal-rendering recipes.</p>
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
     * Locate the top-level {@code > [} only-phi marker in a group body,
     * respecting parentheses and string quoting.
     * @param body Group body (without surrounding parentheses)
     * @return Index of the {@code >}, or -1 if none
     */
    static int topLevelInlinePhi(final String body) {
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

    /**
     * Emit a grouped value that carries an inline {@code > [args]}
     * only-phi formation.
     * @param emit Emitter
     * @param name Name attribute (or {@code null})
     * @param inner Group body (without surrounding parentheses)
     * @param phi Index of the {@code >} marker
     * @param column Source column of the group's opening parenthesis
     * @param line Source line
     * @checkstyle ParameterNumberCheck (5 lines)
     */
    static void inlinePhi(
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
        int pcol = column + bracket + 1;
        for (final String param : InlinePhi.splitParams(params)) {
            Emissions.validParam(param, line, pcol);
            final String mapped;
            if ("@".equals(param)) {
                mapped = "φ";
            } else if ("^".equals(param)) {
                mapped = "ρ";
            } else {
                mapped = param;
            }
            emit.voidParam(mapped, line, pcol);
            pcol = pcol + param.length() + 1;
        }
        final Span sub = new Span(" ".repeat(column).concat(lhs), line);
        Emissions.expression(emit, "φ", new Tokens(sub.body(), sub), line);
        emit.close();
    }

    private static List<String> splitParams(final String text) {
        final List<String> out = new java.util.ArrayList<>(0);
        int idx = 0;
        while (idx < text.length()) {
            int end = idx;
            while (end < text.length() && text.charAt(end) != ' ') {
                end = end + 1;
            }
            out.add(text.substring(idx, end));
            if (end < text.length()) {
                idx = end + 1;
            } else {
                idx = end;
            }
        }
        return out;
    }
}
