/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import java.util.ArrayList;
import java.util.List;

/**
 * A formation line — §3.4 of the spec.
 *
 * <p>Form: {@code [params] [> name [/sig]]}. Each parameter becomes a
 * void child (R-3.4.1). The standalone {@code @} parameter maps to
 * {@code φ} in XMIR (R-3.4.2 / R-9.3). {@code ^} (RHO) is rejected as a
 * parameter name (R-3.4.3). No leading/trailing space inside the
 * brackets (R-3.4.4); exactly one space between parameter names
 * (R-3.4.5). The line may carry an optional name suffix per §3.10,
 * including the atom-signature form {@code > name /sig}. The shorthand
 * {@code ++> name} is accepted as sugar for {@code [] +> name} — a
 * parameterless formation carrying a test suffix (R-6.3.6).</p>
 *
 * <p>Cross-line behaviour: pushes a new {@link Level} at this line's
 * indent (Step C/D) or replaces the current top (Step B), with
 * {@link Kind#BARE_FORMATION} and {@link Openness#OPEN}. The atom flag
 * is set if the suffix carries {@code /sig}; the named flag is set when
 * the suffix is present. *
 *
 * @since 0.1
 */
final class LnFormation implements Line {

    /**
     * The line's source span.
     */
    private final Span span;

    /**
     * Ctor.
     * @param source The source span
     */
    LnFormation(final Span source) {
        this.span = source;
    }

    @Override
    public void into(final Stack stack, final Globals globals, final Emit emit) {
        Blanks.enterAfterMeta(this.span, globals, emit);
        final String body = this.span.body();
        final List<String> params;
        final String binding;
        final Suffix suffix;
        if (body.startsWith("++>")) {
            params = new ArrayList<>(0);
            binding = null;
            suffix = new Suffix(
                body.substring(1), this.span, this.span.indent() + 1
            );
        } else {
            final int close = LnFormation.findClosing(body, this.span);
            params = LnFormation.params(body, close, this.span);
            final String raw = body.substring(close + 1);
            binding = LnFormation.outerBinding(raw);
            final String tail;
            if (binding == null) {
                tail = raw;
            } else {
                tail = raw.substring(1 + binding.length());
            }
            suffix = new Suffix(
                tail, this.span,
                this.span.indent() + close + 1 + LnFormation.bindingWidth(binding)
            );
        }
        if (suffix.test()) {
            Blanks.checkTest(this.span, globals, emit);
        }
        Comments.seal(globals, emit, this.span);
        this.transition(stack, suffix);
        globals.clearBlanks();
        globals.markEmitted();
        this.emit(emit, suffix, params, binding);
    }

    /**
     * Extract a leading {@code :label} inline binding from the tail
     * that follows the closing {@code ]}. Returns the bare label (no
     * leading colon) or {@code null} when the tail does not start
     * with {@code :}.
     * @param raw The post-{@code ]} substring
     * @return Binding label, or {@code null}
     */
    private static String outerBinding(final String raw) {
        final String label;
        if (raw.startsWith(":")) {
            int idx = 1;
            while (idx < raw.length() && raw.charAt(idx) != ' ' && raw.charAt(idx) != '>') {
                idx = idx + 1;
            }
            label = raw.substring(1, idx);
        } else {
            label = null;
        }
        return label;
    }

    /**
     * Source-column width of a {@code :label} binding (label length
     * plus the leading colon), or {@code 0} when no binding is
     * present.
     * @param binding The binding label, or {@code null}
     * @return Width in characters
     */
    private static int bindingWidth(final String binding) {
        final int width;
        if (binding == null) {
            width = 0;
        } else {
            width = binding.length() + 1;
        }
        return width;
    }

    /**
     * Push or replace the stack level per Step B/C/D of §5.2.
     * @param stack The stack
     * @param suffix The parsed suffix (sets named/atom flags)
     */
    private void transition(final Stack stack, final Suffix suffix) {
        final Level level;
        if (stack.empty() || stack.top().indent() < this.span.indent()) {
            this.checkChildAllowed(stack, suffix);
            level = stack.push(
                this.span.indent(), this.span.line(),
                Kind.BARE_FORMATION, Openness.OPEN
            );
        } else {
            level = stack.replace(
                this.span.line(), Kind.BARE_FORMATION, Openness.OPEN
            );
        }
        if (suffix.present()) {
            level.name(suffix.label());
        }
        if (suffix.atom()) {
            level.mark();
        }
    }

    /**
     * Validate that a deeper-indent formation is legal under its
     * pending parent — indent step is exactly one, parent is open, and
     * (per R-3.10.13) the parent is not an atom unless this child is
     * itself a test attribute.
     * @param stack The stack
     * @param suffix The parsed suffix
     */
    private void checkChildAllowed(final Stack stack, final Suffix suffix) {
        if (!stack.empty()
            && this.span.indent() != stack.top().indent() + 2) {
            throw new ParseError(
                this.span.line(), 0,
                "indent increased by more than one level"
            );
        }
        if (!stack.empty()
            && stack.top().openness() != Openness.OPEN) {
            throw new ParseError(
                this.span.line(), 0,
                "unexpected deeper-indent line — previous expression is closed for children"
            );
        }
        if (!stack.empty() && stack.top().atom() && !suffix.test()) {
            throw new ParseError(
                this.span.line(), this.span.indent(),
                "Atom cannot contain inner objects; only `+>` test attributes are allowed in an atom body"
            );
        }
    }

    /**
     * Emit the formation's {@code <o>}, void params, and (if atom) the
     * {@code λ} marker. The cursor remains inside the new {@code <o>}
     * so deeper-indent children attach as siblings of the voids.
     * @param emit The directives sink
     * @param suffix The parsed suffix
     * @param params Parameter names in source order
     * @param binding Outer inline-binding label, or {@code null}
     * @checkstyle ParameterNumberCheck (3 lines)
     */
    private void emit(
        final Emit emit, final Suffix suffix, final List<String> params, final String binding
    ) {
        emit.object(
            suffix.attribute(this.span.line(), this.span.indent()),
            null, this.span.line(), this.span.indent()
        );
        if (!suffix.handle().isEmpty()) {
            emit.local(suffix.handle());
        }
        if (binding != null) {
            emit.slot(Emissions.bindingTag(binding));
        }
        if (suffix.constant()) {
            emit.constant();
        }
        int column = this.span.indent() + 1;
        for (final String param : params) {
            emit.voidParam(param, this.span.line(), column);
            column = column + param.length() + 1;
        }
        if (suffix.atom()) {
            emit.atomMarker(suffix.sig(), this.span.line(), this.span.indent());
        }
    }

    /**
     * Locate the matching {@code ]} for the leading {@code [}.
     * @param body The line body
     * @param span The source span (for error)
     * @return Index of the closing bracket
     */
    private static int findClosing(final String body, final Span span) {
        if (body.isEmpty() || body.charAt(0) != '[') {
            throw new ParseError(
                span.line(), span.indent(),
                "formation must start with `[`"
            );
        }
        final int close = body.indexOf(']');
        if (close < 0) {
            throw new ParseError(
                span.line(), span.indent(),
                "formation brackets must not contain leading or trailing space"
            );
        }
        return close;
    }

    /**
     * Parse the parameter list between the brackets per R-3.4.x.
     * @param body The line body
     * @param close Index of {@code ]}
     * @param span The source span (for error)
     * @return Parameter names in source order
     */
    private static List<String> params(
        final String body, final int close, final Span span
    ) {
        final String inside = body.substring(1, close);
        final List<String> out = new ArrayList<>(2);
        if (!inside.isEmpty()
            && (inside.charAt(0) == ' ' || inside.charAt(inside.length() - 1) == ' ')) {
            throw new ParseError(
                span.line(), span.indent() + 1,
                "formation brackets must not contain leading or trailing space"
            );
        }
        int idx = 0;
        while (idx < inside.length()) {
            int end = idx;
            while (end < inside.length() && inside.charAt(end) != ' ') {
                end = end + 1;
            }
            final String raw = inside.substring(idx, end);
            out.add(LnFormation.mapParam(raw, span, span.indent() + 1 + idx));
            if (end < inside.length()) {
                if (end + 1 < inside.length() && inside.charAt(end + 1) == ' ') {
                    throw new ParseError(
                        span.line(), span.indent() + 1 + end,
                        "parameter names in voids must be separated by exactly one space"
                    );
                }
                idx = end + 1;
            } else {
                idx = end;
            }
        }
        return out;
    }

    /**
     * Translate a raw parameter token to its emitted name. {@code @}
     * maps to {@code φ} per R-3.4.2 / R-9.3; {@code ^} is rejected per
     * R-3.4.3.
     * @param raw Raw token
     * @param span Source span
     * @param pos Source column of the token
     * @return Emitted name
     */
    private static String mapParam(final String raw, final Span span, final int pos) {
        if (raw.equals("^")) {
            throw new ParseError(
                span.line(), pos,
                "parameter names in voids must be NAME or @"
            );
        }
        final String mapped;
        if (raw.equals("@")) {
            mapped = "φ";
        } else {
            mapped = raw;
        }
        return mapped;
    }
}
