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
 * the caller enforces the step (R-5.2.7) before calling
 * {@link #push(int, int, Kind, Openness)}. The bottom entry's
 * {@code parent} is always {@link Kind#TOP_LEVEL}; higher entries carry the
 * kind of the entry directly below them as their parent.</p>
 *
 * <p>This class manages structural transitions only. Close-time semantic
 * checks (R-5.3.1 through R-5.3.5) are dispatched to a caller-supplied
 * {@link Closer} when a level is popped or replaced, so the same checks
 * fire whether closing happens mid-parse (a shallower or sibling line
 * arrived) or at end-of-stream (R-5.4 / §8). The stack itself is
 * structural; semantics live with the caller.</p>
 *
 * @since 0.1
 */
final class Stack {

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
     * Bottom sentinel — the entry {@link #below()} answers when the
     * stack holds fewer than two entries. Its kind is
     * {@link Kind#TOP_LEVEL}, so a caller reading the parent of the
     * bottom entry finds an object saying "top level" rather than an
     * absence to test for.
     */
    private final Level bottom;

    /**
     * Ctor with no-op hooks — useful in tests that exercise structural
     * transitions without semantic checks.
     */
    Stack() {
        this((level, naming) -> { }, level -> { });
    }

    /**
     * Ctor with only a closer; opener defaults to no-op.
     * @param hook Close-time check hook
     */
    Stack(final Closer hook) {
        this(hook, level -> { });
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
        this.bottom = new Level(0, 0, Kind.TOP_LEVEL, Openness.OPEN, Kind.TOP_LEVEL, false);
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
     * A copy of the current levels — savepoint for per-line rollback
     * (R-7.3). A plain entry count cannot serve as that savepoint:
     * {@link #replace(int, Kind, Openness)} removes one entry and adds
     * one back, so the count is unchanged even though the top entry
     * itself is a different object. Snapshotting the entries themselves
     * survives that case.
     *
     * <p>The entries are copied, not shared: a {@link Level} is mutable
     * and a line that throws has usually already mutated the entry
     * below it, so a shallow copy would put the damage straight back on
     * the stack.</p>
     *
     * @return Snapshot of the levels, bottom-to-top
     */
    List<Level> snapshot() {
        final List<Level> copy = new ArrayList<>(this.levels.size());
        for (final Level level : this.levels) {
            copy.add(level.twin());
        }
        return copy;
    }

    /**
     * Restore the levels to a previously taken {@link #snapshot()},
     * discarding whatever {@link #push} / {@link #replace} side effects
     * happened since. Used by {@link Eo} to undo those side effects for
     * a line that threw a {@link ParseError} (R-7.3) — the closer is
     * <em>not</em> invoked here because the rolled-back open directives
     * never reached the sink.
     * @param snapshot A snapshot taken earlier via {@link #snapshot()}
     */
    void restore(final List<Level> snapshot) {
        this.levels.clear();
        this.levels.addAll(snapshot);
    }

    /**
     * The entry directly below the top, or the bottom sentinel when the
     * stack holds fewer than two entries. Used by the FSM to read a new
     * entry's parent during the push step (R-5.2.8).
     * @return Entry below top, never null
     */
    Level below() {
        final Level under;
        if (this.levels.size() < 2) {
            under = this.bottom;
        } else {
            under = this.levels.get(this.levels.size() - 2);
        }
        return under;
    }

    /**
     * The outermost entry — the indent-0 top-level object of the file —
     * or the bottom sentinel when nothing has been pushed yet. Read by
     * R-6.3.3, which admits a {@code +>} test attribute only under a
     * top-level object that is a formation.
     * @return Outermost entry, never null
     */
    Level root() {
        final Level outer;
        if (this.levels.isEmpty()) {
            outer = this.bottom;
        } else {
            outer = this.levels.get(0);
        }
        return outer;
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
     * @return The pushed level
     * @checkstyle ParameterNumberCheck (3 lines)
     */
    Level push(
        final int indent, final int line, final Kind kind, final Openness openness
    ) {
        final Kind parent;
        final boolean patom;
        final boolean argues;
        final String owner;
        if (this.levels.isEmpty()) {
            if (indent != 0) {
                throw new ParseError(
                    line, indent, "unexpected indentation, the first object must start at indent 0"
                );
            }
            parent = Kind.TOP_LEVEL;
            patom = false;
            argues = false;
            owner = "";
        } else {
            final Level under = this.top();
            parent = under.kind();
            patom = under.atom();
            argues = under.argumentative();
            owner = under.governingFormation();
            under.observeVoid(kind, line, indent);
        }
        if (!this.levels.isEmpty()) {
            this.opener.beforeChild(this.top());
        }
        final Level fresh = new Level(indent, line, kind, openness, parent, patom);
        if (argues) {
            fresh.argues(owner);
        }
        this.levels.add(fresh);
        if (parent == Kind.BARE_REVERSED || parent == Kind.ONLY_PHI) {
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
     * indent {@code target} - 2 (a step occurred), its openness is
     * downgraded from {@link Openness#OPEN OPEN} to
     * {@link Openness#VCOMPLETED VCOMPLETED} per R-5.2.2.
     * @param target Target indent
     */
    void popDeeperThan(final int target) {
        boolean stepped = false;
        while (!this.levels.isEmpty() && this.top().indent() > target) {
            final Level last = this.levels.remove(this.levels.size() - 1);
            stepped = true;
            this.closer.onClose(last, true);
        }
        if (stepped && !this.levels.isEmpty() && this.top().openness() == Openness.OPEN) {
            this.top().close(Openness.VCOMPLETED);
        }
    }

    /**
     * Replace the top entry with a fresh one at the same indent
     * (R-5.2.4), invoking the closer on the entry being replaced. The
     * new entry's {@code parent} comes from the entry below.
     * @param line Start line of the new entry
     * @param kind Initial outer kind
     * @param openness Initial openness
     * @return The new top
     */
    Level replace(final int line, final Kind kind, final Openness openness) {
        if (this.levels.isEmpty()) {
            throw new IllegalStateException("cannot replace top of empty stack");
        }
        final Level old = this.levels.remove(this.levels.size() - 1);
        this.closer.onClose(old, true);
        final int indent = old.indent();
        final Kind parent;
        final boolean patom;
        final boolean argues;
        final String owner;
        if (this.levels.isEmpty()) {
            parent = Kind.TOP_LEVEL;
            patom = false;
            argues = false;
            owner = "";
        } else {
            final Level under = this.top();
            parent = under.kind();
            patom = under.atom();
            argues = under.argumentative();
            owner = under.governingFormation();
            under.observeVoid(kind, line, indent);
            this.opener.beforeChild(under);
        }
        final Level fresh = new Level(indent, line, kind, openness, parent, patom);
        if (argues) {
            fresh.argues(owner);
        }
        this.levels.add(fresh);
        return fresh;
    }

    /**
     * Run the closer on the top entry without popping it, so the entry
     * can be re-purposed at the same indent (R-5.2.5): a same-indent
     * {@code .method} continuation closes what stands above it and
     * takes its place. Everything the closer does on a pop happens
     * here too — the argument-binding check, the bare-reversed
     * receiver check, the compact-tuple wrapper and count — and the
     * entry is left with none of that state, since it is a plain
     * dispatch from now on.
     */
    void seal() {
        final Level level = this.top();
        this.closer.onClose(level, false);
        level.sealed();
    }

    /**
     * Pop every remaining entry and run the closer on each — used by EOF
     * (§8).
     */
    void close() {
        while (!this.levels.isEmpty()) {
            final Level last = this.levels.remove(this.levels.size() - 1);
            this.closer.onClose(last, true);
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
         * Run close-time checks on a popped, replaced or sealed level.
         * @param level The level being closed
         * @param naming Whether the naming requirement applies to it
         */
        void onClose(Level level, boolean naming);
    }

    /**
     * Pre-child hook.
     *
     * <p>Invoked just before a new level is pushed under an existing
     * parent. Used to inject pre-child emissions — the canonical case
     * is the compact-tuple wrapper: once a {@link Kind#COMPACT_TUPLE}
     * parent has accumulated N direct children, the next child must
     * land inside a synthesised {@code <o base='Φ.tuple' star=''>}
     * wrapper. The opener emits that wrapper exactly once.</p>
     *
     * @since 0.1
     */
    @FunctionalInterface
    interface Opener {

        /**
         * React to a new child being pushed under {@code parent}.
         * @param parent The parent level (current top before push)
         */
        void beforeChild(Level parent);
    }
}
