/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

/**
 * The stack transition triggered when a {@link Line} adopts the
 * parser cursor — §5.2 (Step B/C/D) of the spec.
 *
 * <p>Every line-shape parser (application, reversed, compact-tuple,
 * inline-phi, …) replays the same prologue when it owns the cursor:
 * if the new line is deeper-indent it pushes a fresh {@link Level}
 * after checking the indent step (R-5.2.7) and the parent's openness
 * (R-5.2.4); otherwise it replaces the level on top at the same
 * indent. Either way, if the new line carries a naming suffix the
 * level is flagged as named (R-5.3.1). This class is the single
 * source of truth for that prologue.</p>
 *
 * @since 0.1
 */
final class Transition {

    /**
     * The indent stack to push/replace on.
     */
    private final Stack stack;

    /**
     * The line being adopted.
     */
    private final Span span;

    /**
     * Ctor.
     * @param stk The indent stack
     * @param src The line span being adopted
     */
    Transition(final Stack stk, final Span src) {
        this.stack = stk;
        this.span = src;
    }

    /**
     * Push a fresh level (when stepping deeper) or replace the level
     * on top (when staying at the same indent), and mark it named if
     * {@code admission} carries a label.
     * @param kind Outer kind for the level
     * @param openness Openness for the level
     * @param admission The line's naming suffix and atom-body permission
     * @return The pushed-or-replaced level
     */
    Level apply(final Kind kind, final Openness openness, final Admission admission) {
        final Level level;
        if (this.stack.empty() || this.stack.top().indent() < this.span.indent()) {
            level = this.pushed(kind, openness);
        } else {
            level = this.stack.replace(this.span.line(), kind, openness);
        }
        if (level.patom() && !admission.permitted()) {
            throw new ParseError(
                this.span.line(), this.span.indent(), admission.violation()
            );
        }
        admission.name(level);
        return level;
    }

    private Level pushed(final Kind kind, final Openness openness) {
        if (!this.stack.empty() && this.span.indent() != this.stack.top().indent() + 2) {
            throw new ParseError(
                this.span.line(), 0,
                "indent increased by more than one level"
            );
        }
        if (!this.stack.empty()) {
            final Level top = this.stack.top();
            if (top.openness() != Openness.OPEN) {
                throw new ParseError(
                    this.span.line(), 0,
                    "unexpected deeper-indent line — previous expression is closed for children"
                );
            }
            if (top.kind() == Kind.HEAD || top.kind() == Kind.HMETHOD) {
                top.become(Kind.VAPPLICATION);
            }
        }
        return this.stack.push(this.span.indent(), this.span.line(), kind, openness);
    }
}
