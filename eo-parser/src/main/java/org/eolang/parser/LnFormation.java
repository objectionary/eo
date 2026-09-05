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
 * {@code φ} in XMIR (R-3.4.2 / R-9.3). The standalone {@code ^} maps to
 * {@code ρ} and declares the formation's receiver; it may stand in any
 * position among the parameters (R-3.4.3 / R-3.4.11). No leading/trailing space inside the
 * brackets (R-3.4.4); exactly one space between parameter names
 * (R-3.4.5). The line may carry an optional name suffix per §3.10,
 * including the atom-signature form {@code > name /sig}. The shorthand
 * {@code ++> name} is accepted as sugar for {@code [] +> name} — a
 * parameterless formation carrying a truthy test suffix (R-6.3.6); its
 * throwing counterpart {@code --> name} is sugar for {@code [] -> name}.</p>
 *
 * <p>Cross-line behaviour: pushes a new {@link Level} at this line's
 * indent (Step C/D) or replaces the current top (Step B), with
 * {@link Kind#BARE_FORMATION} and {@link Openness#OPEN}. The atom flag
 * is set if the suffix carries {@code /sig}; the named flag is set when
 * the suffix is present.</p>
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
        final String body = this.span.body();
        final List<String> params;
        final String binding;
        final Suffix suffix;
        if (body.startsWith("++>") || body.startsWith("-->")) {
            params = new ArrayList<>(0);
            binding = "";
            suffix = new Suffix(
                body.substring(1), this.span, this.span.indent() + 1
            );
        } else {
            final int close = LnFormation.findClosing(body, this.span);
            params = LnFormation.params(body, close, this.span);
            final String raw = body.substring(close + 1);
            binding = LnFormation.outerBinding(
                raw, this.span, this.span.indent() + close + 2
            );
            final int width = LnFormation.bindingWidth(binding);
            suffix = new Suffix(
                raw.substring(width), this.span,
                this.span.indent() + close + 1 + width
            );
        }
        this.checkAtomVoids(suffix, params);
        if (suffix.test()) {
            Blanks.checkTest(this.span, stack, globals, emit);
        }
        Blanks.enterAfterMeta(this.span, globals, emit);
        globals.seal(emit, this.span);
        this.transition(stack, suffix);
        Bindings.observeChild(stack, binding, this.span);
        globals.clearBlanks();
        globals.markEmitted();
        this.emit(emit, suffix, params, binding);
    }

    private static String outerBinding(final String raw, final Span span, final int pos) {
        final String label;
        if (raw.startsWith(":")) {
            int idx = 1;
            while (idx < raw.length() && raw.charAt(idx) != ' ' && raw.charAt(idx) != '>') {
                idx = idx + 1;
            }
            label = raw.substring(1, idx);
            Tokens.checkBinding(label, span, pos);
        } else {
            label = "";
        }
        return label;
    }

    private static int bindingWidth(final String binding) {
        final int width;
        if (binding.isEmpty()) {
            width = 0;
        } else {
            width = binding.length() + 1;
        }
        return width;
    }

    private void checkAtomVoids(final Suffix suffix, final List<String> params) {
        if (suffix.atom() && !params.isEmpty()) {
            throw new ParseError(
                this.span.line(), this.span.indent() + 1,
                "an atom must declare its void attributes vertically, as ? > name lines"
            );
        }
    }

    private void transition(final Stack stack, final Suffix suffix) {
        final Level level = new Transition(stack, this.span).apply(
            Kind.BARE_FORMATION, Openness.OPEN,
            new Admission(suffix.named(), suffix.test(), suffix.atom(), suffix.test())
        );
        if (suffix.atom()) {
            level.mark();
        }
    }

    private void emit(
        final Emit emit, final Suffix suffix, final List<String> params, final String binding
    ) {
        emit.baselessObject(
            suffix.attribute(this.span.line(), this.span.indent()),
            this.span.line(), this.span.indent()
        );
        if (!suffix.handle().isEmpty()) {
            emit.local(suffix.handle());
        }
        if (!binding.isEmpty()) {
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

    private static int findClosing(final String body, final Span span) {
        final int close = body.indexOf(']');
        if (close < 0) {
            throw new ParseError(
                span.line(), span.indent(),
                "formation is missing its closing bracket"
            );
        }
        return close;
    }

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
            out.add(
                LnFormation.mapParam(raw, span, span.indent() + 1 + idx)
            );
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

    private static String mapParam(final String raw, final Span span, final int pos) {
        Emissions.validParam(raw, span.line(), pos);
        return new VoidName(raw).asString();
    }
}
