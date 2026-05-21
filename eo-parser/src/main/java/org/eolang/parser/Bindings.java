/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import java.util.List;

/**
 * Cross-argument inline-binding validation — §6.6 of the spec.
 *
 * <p>Within a same-indent group of application arguments, bindings
 * must be all-bound or all-unbound (R-6.6.2). The receiver of a
 * reversed dispatch is exempt from this rule and cannot itself carry
 * a binding (R-6.6.3).</p>
 *
 * <p>This class collects the small rule set as static helpers shared
 * by every line shape that reads an argument group — {@link
 * LnApplication}, {@link LnMethod}, {@link LnReversed}, and the
 * compact-tuple flavours.</p>
 *
 * @since 0.1
 */
final class Bindings {

    /**
     * No instances.
     */
    private Bindings() {
    }

    /**
     * Verify the all-or-nothing rule across an argument group.
     *
     * <p>The args must either all carry a binding or none. When the
     * mix is non-uniform, throws a {@link ParseError} at the column of
     * the first divergent arg — i.e., the first arg whose
     * binding-presence differs from the group's first arg's.</p>
     *
     * <p>An empty list or a single arg is always valid.</p>
     *
     * @param args The argument group
     * @param span Source span (for error positioning)
     */
    static void checkAllOrNothing(final List<Value> args, final Span span) {
        if (args.size() >= 2) {
            final boolean head = args.get(0).binding() != null;
            for (int idx = 1; idx < args.size(); idx = idx + 1) {
                final boolean bound = args.get(idx).binding() != null;
                if (bound != head) {
                    throw new ParseError(
                        span.line(), args.get(idx).pos(),
                        "argument bindings must be all-or-nothing"
                    );
                }
            }
        }
    }

    /**
     * Verify that a reversed-dispatch receiver carries no binding —
     * R-6.6.3.
     * @param receiver The receiver value (first arg of a horizontal
     *  reversed line)
     * @param span Source span
     */
    static void checkReceiver(final Value receiver, final Span span) {
        if (receiver.binding() != null) {
            throw new ParseError(
                span.line(), receiver.pos(),
                "reversed-dispatch receiver cannot carry a binding"
            );
        }
    }

    /**
     * Observe a freshly-pushed child line's outer binding against its
     * parent context — cross-line R-6.6.2 / R-6.6.3 / R-3.12.3.
     *
     * <p>For arg-bearing parents ({@code HEAD}, {@code HMETHOD},
     * {@code VAPPLICATION}) the binding presence updates the parent's
     * binding mode and rejects mismatches. For {@code BARE_REVERSED}
     * parents the first child is the receiver (no binding allowed);
     * subsequent children participate in the all-or-nothing group. For
     * formation / top-level parents, any binding is rejected per
     * R-3.12.3.</p>
     *
     * @param stack Indent stack (the new child is already on top)
     * @param outer Outer binding label, or {@code null} when absent
     * @param span Source span of the child line
     */
    static void observeChild(final Stack stack, final String outer, final Span span) {
        final Level parent = stack.below();
        if (parent == null || parent.kind() == Kind.BARE_FORMATION) {
            Bindings.rejectBinding(outer, span);
        } else if (parent.kind() == Kind.BARE_REVERSED) {
            Bindings.observeReversedChild(parent, outer, span);
        } else if (Bindings.tracksBindings(parent.kind())) {
            parent.observeBinding(outer != null, span);
        }
    }

    /**
     * Whether a parent kind is an arg-bearing context where the
     * all-or-nothing binding rule applies to deeper children.
     * @param kind Parent kind
     * @return True if the parent tracks binding consistency
     */
    private static boolean tracksBindings(final Kind kind) {
        return kind == Kind.HEAD || kind == Kind.HMETHOD || kind == Kind.VAPPLICATION;
    }

    /**
     * Reject a binding on a child in a context that disallows it —
     * formation body or top-level (R-3.12.3).
     * @param outer Outer binding (may be {@code null})
     * @param span Source span of the child
     */
    private static void rejectBinding(final String outer, final Span span) {
        if (outer != null) {
            throw new ParseError(
                span.line(), span.indent(),
                "binding allowed only in argument position"
            );
        }
    }

    /**
     * Handle a child under a {@link Kind#BARE_REVERSED} parent. The
     * first child is the receiver and must not carry a binding
     * (R-6.6.3); subsequent children participate in the
     * all-or-nothing group (R-6.6.2).
     * @param parent The bare-reversed parent
     * @param outer Outer binding (may be {@code null})
     * @param span Source span of the child
     */
    private static void observeReversedChild(
        final Level parent, final String outer, final Span span
    ) {
        if (parent.children() > 1) {
            parent.observeBinding(outer != null, span);
        } else if (outer != null) {
            throw new ParseError(
                span.line(), span.indent(),
                "reversed-dispatch receiver cannot carry a binding"
            );
        }
    }
}
