/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import java.util.ArrayList;
import java.util.List;

/**
 * Indent stack — §5 of the spec.
 *
 * <p>The stack carries one {@link Level} per occupied indent level. Indents
 * grow strictly bottom-to-top in steps of exactly two (R-5.1.1, R-5.1.2);
 * this class enforces the step at {@link #push(int, int, Kind, Openness)}.
 * The bottom entry's {@code parent} is always {@link Kind#TOP_LEVEL};
 * higher entries carry the kind of the entry directly below them as their
 * parent.</p>
 *
 * <p>This class manages structural transitions only. Close-time semantic
 * checks (R-5.3.1 through R-5.3.5) are dispatched to a caller-supplied
 * {@link Closer} when a level is popped or replaced, so the same checks
 * fire whether closing happens mid-parse (a shallower or sibling line
 * arrived) or at end-of-stream (R-5.4 / §8). The stack itself is
 * structural; semantics live with the caller. *
 *
 * @since 0.1
 */
final class Stack {

    /**
     * Indent step between adjacent stack entries — fixed at 2 spaces by
     * R-2.2.1.
     */
    private static final int STEP = 2;

    /**
     * The levels, bottom-to-top.
     */
    private final List<Level> levels;

    /**
     * Close-time check hook, invoked whenever a level is popped or
     * replaced. Injected once at construction so per-call wiring stays
     * out of the API.
     */
    private final Closer closer;

    /**
     * Pre-child hook, invoked just before a new level is pushed under
     * an existing top. Lets the owner inject directives (e.g., the
     * compact-tuple {@code Φ.tuple} wrapper) before the child opens.
     */
    private final Opener opener;

    /**
     * Ctor with no-op hooks — useful in tests that exercise structural
     * transitions without semantic checks.
     */
    Stack() {
        this(level -> { }, (level, named) -> { });
    }

    /**
     * Ctor with only a closer; opener defaults to no-op.
     * @param hook Close-time check hook
     */
    Stack(final Closer hook) {
        this(hook, (level, named) -> { });
    }

    /**
     * Primary ctor.
     * @param closer Close-time check hook
     * @param opener Pre-child hook
     */
    Stack(final Closer closer, final Opener opener) {
        this.levels = new ArrayList<>(8);
        this.closer = closer;
        this.opener = opener;
    }

    /**
     * Whether the stack has no entries.
     * @return True if empty
     */
    boolean empty() {
        return this.levels.isEmpty();
    }

    /**
     * Number of entries currently on the stack.
     * @return Depth
     */
    int depth() {
        return this.levels.size();
    }

    /**
     * The top entry.
     * @return Top
     */
    Level top() {
        if (this.levels.isEmpty()) {
            throw new IllegalStateException("stack is empty — no top entry");
        }
        return this.levels.get(this.levels.size() - 1);
    }

    /**
     * Current number of entries on the stack — savepoint for per-line
     * rollback (R-7.3).
     * @return Entry count
     */
    int size() {
        return this.levels.size();
    }

    /**
     * Pop entries silently until {@code size()} equals {@code target}.
     * Used by {@link Eo} to undo {@link #push} / {@link #replace} side
     * effects of a line that threw a {@link ParseError} (R-7.3) — the
     * closer is <em>not</em> invoked here because the rolled-back open
     * directives never reached the sink.
     * @param target Target stack size
     */
    void silentTruncate(final int target) {
        while (this.levels.size() > target) {
            this.levels.remove(this.levels.size() - 1);
        }
    }

    /**
     * The entry directly below the top, or null if there is none. Used by
     * the FSM to read a new entry's parent during the push step (R-5.2.8).
     * @return Entry below top, or null
     */
    Level below() {
        final Level under;
        if (this.levels.size() < 2) {
            under = null;
        } else {
            under = this.levels.get(this.levels.size() - 2);
        }
        return under;
    }

    /**
     * Push a fresh level at the given indent.
     *
     * <p>If the stack is non-empty, the new indent must equal {@code
     * top().indent() + 2} (R-5.2.7). If empty, the new indent must be 0
     * (a top-level entry per R-5.2.11). The new entry's
     * {@code parent} is read from the entry below; if the stack was
     * empty, the parent is {@link Kind#TOP_LEVEL}.</p>
     *
     * @param indent New indent
     * @param line Start line
     * @param kind Initial outer kind
     * @param openness Initial openness
     * @param named Whether the incoming line carries a name suffix
     * @return The pushed level
     * @checkstyle ParameterNumberCheck (3 lines)
     */
    Level push(
        final int indent, final int line, final Kind kind, final Openness openness,
        final boolean named
    ) {
        final Kind parent;
        final boolean patom;
        if (this.levels.isEmpty()) {
            if (indent != 0) {
                throw new IllegalStateException(
                    String.format(
                        "first push must be at indent 0, was %d", indent
                    )
                );
            }
            parent = Kind.TOP_LEVEL;
            patom = false;
        } else {
            final Level under = this.top();
            if (indent != under.indent() + Stack.STEP) {
                throw new IllegalStateException(
                    String.format(
                        "push at indent %d violates step rule from indent %d",
                        indent, under.indent()
                    )
                );
            }
            parent = under.kind();
            patom = under.atom();
            under.observeVoid(kind, line, indent);
        }
        if (!this.levels.isEmpty()) {
            this.opener.beforeChild(this.top(), named);
        }
        final Level fresh = new Level(indent, line, kind, openness, parent, patom);
        this.levels.add(fresh);
        if (parent == Kind.BARE_REVERSED) {
            final Level host = this.levels.get(this.levels.size() - 2);
            if (!host.taken()) {
                host.consumeReceiver();
            }
        }
        return fresh;
    }

    /**
     * Pop every entry whose indent is strictly greater than
     * {@code target} (R-5.2.1), invoking the constructor-supplied closer
     * on each as it is removed. After the pop sweep, if the new top has
     * indent {@code target} − 2 (a step occurred), its openness is
     * downgraded from {@link Openness#OPEN OPEN} to
     * {@link Openness#VERTICAL_COMPLETED VERTICAL_COMPLETED} per
     * R-5.2.2.
     * @param target Target indent
     */
    void popDeeperThan(final int target) {
        boolean stepped = false;
        while (!this.levels.isEmpty() && this.top().indent() > target) {
            final Level last = this.levels.remove(this.levels.size() - 1);
            stepped = true;
            this.closer.onClose(last);
        }
        if (stepped && !this.levels.isEmpty() && this.top().openness() == Openness.OPEN) {
            this.top().close(Openness.VERTICAL_COMPLETED);
        }
    }

    /**
     * Replace the top entry with a fresh one at the same indent
     * (R-5.2.4), invoking the closer on the entry being replaced. The
     * new entry's {@code parent} comes from the entry below.
     * @param line Start line of the new entry
     * @param kind Initial outer kind
     * @param openness Initial openness
     * @param named Whether the incoming line carries a name suffix
     * @return The new top
     */
    Level replace(
        final int line, final Kind kind, final Openness openness, final boolean named
    ) {
        if (this.levels.isEmpty()) {
            throw new IllegalStateException("cannot replace top of empty stack");
        }
        final Level old = this.levels.remove(this.levels.size() - 1);
        this.closer.onClose(old);
        final int indent = old.indent();
        final Kind parent;
        final boolean patom;
        if (this.levels.isEmpty()) {
            parent = Kind.TOP_LEVEL;
            patom = false;
        } else {
            final Level under = this.top();
            parent = under.kind();
            patom = under.atom();
            under.observeVoid(kind, line, indent);
            this.opener.beforeChild(under, named);
        }
        final Level fresh = new Level(indent, line, kind, openness, parent, patom);
        this.levels.add(fresh);
        return fresh;
    }

    /**
     * Pop every remaining entry and run the closer on each — used by EOF
     * (§8).
     */
    void close() {
        while (!this.levels.isEmpty()) {
            final Level last = this.levels.remove(this.levels.size() - 1);
            this.closer.onClose(last);
        }
    }

    /**
     * Close-time check hook.
     *
     * <p>Invoked whenever a level is popped or replaced. The
     * implementation runs the §5.3 semantic checks: naming requirement
     * (R-5.3.1), bare-reversed completeness (R-5.3.2), compact-tuple
     * count (R-5.3.3), atom body restrictions (R-5.3.4, R-5.3.5), and
     * only-phi closure violation (R-5.3.6). The hook is structural,
     * not part of the stack mechanics — same shape regardless of whether
     * the pop is mid-parse or at EOF.</p>
     *
     * @since 0.1
     */
    @FunctionalInterface
    interface Closer {

        /**
         * Run close-time checks on a popped or replaced level.
         * @param level The level being closed
         */
        void onClose(Level level);
    }

    /**
     * Pre-child hook.
     *
     * <p>Invoked just before a new level is pushed under an existing
     * parent. Used to inject pre-child emissions — the canonical case
     * is the compact-tuple wrapper: once a {@link Kind#COMPACT_TUPLE}
     * parent has accumulated N direct children, the next child must
     * land inside a synthesised {@code <o base='Φ.tuple' star=''>}
     * wrapper. The opener emits that wrapper exactly once. It also
     * closes an only-phi formation's φ decoratee when the incoming child
     * is named, so a named body line becomes a sibling attribute of the
     * formation rather than a further φ argument (§4.5).</p>
     *
     * @since 0.1
     */
    @FunctionalInterface
    interface Opener {

        /**
         * React to a new child being pushed under {@code parent}.
         * @param parent The parent level (current top before push)
         * @param named Whether the incoming child carries a name suffix
         */
        void beforeChild(Level parent, boolean named);
    }
}
