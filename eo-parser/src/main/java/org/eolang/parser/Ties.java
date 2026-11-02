/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import java.util.Collections;
import java.util.HashSet;
import java.util.Optional;
import java.util.Set;

/**
 * The inline bindings one argument group has carried so far — R-6.6.2
 * and R-6.6.5 of the spec.
 *
 * <p>An argument group holds exactly one kind of argument: none of them
 * bound ({@code foo 1 2}), all of them bound by slot number
 * ({@code foo 1:0 2:1}), or all of them bound by name
 * ({@code foo 1:a 2:b}). The first argument fixes the kind, every later
 * one must match it, and no label may be bound twice in the group.</p>
 *
 * <p>Immutable: each argument yields the next state of the group, so
 * {@link Bindings} folds a horizontal group through it in one pass and a
 * {@link Level} keeps the state of a vertical group across its lines,
 * sharing it with its savepoint twin.</p>
 *
 * @since 0.64.0
 */
final class Ties {

    /**
     * Labels the group has bound so far.
     */
    private final Set<String> labels;

    /**
     * Kind of the group, fixed by its first argument.
     */
    private final Mode mode;

    /**
     * Ctor — a group with no argument yet.
     */
    Ties() {
        this(Mode.UNSET, Collections.emptySet());
    }

    /**
     * Primary ctor.
     *
     * @param kind Kind of the group
     * @param seen Labels the group has bound so far
     */
    private Ties(final Mode kind, final Set<String> seen) {
        this.mode = kind;
        this.labels = seen;
    }

    /**
     * The group with one more argument, rejecting an argument whose
     * kind differs from the group's (R-6.6.2) or whose label the group
     * already binds (R-6.6.5).
     *
     * @param tie The argument's binding label, absent when it carries none
     * @param line Source line of the argument (for the error)
     * @param pos Source column of the argument (for the error)
     * @return The group with the argument added
     */
    Ties with(final Optional<String> tie, final int line, final int pos) {
        return new Ties(this.kind(tie, line, pos), this.joined(tie, line, pos));
    }

    private Mode kind(final Optional<String> tie, final int line, final int pos) {
        final Mode kind;
        if (tie.isEmpty()) {
            kind = Mode.NONE;
        } else if (!tie.get().isEmpty()
            && tie.get().chars().allMatch(glyph -> glyph >= '0' && glyph <= '9')) {
            kind = Mode.SLOT;
        } else {
            kind = Mode.NAME;
        }
        if (this.mode != Mode.UNSET && this.mode != kind) {
            final String message;
            if (this.mode == Mode.NONE || kind == Mode.NONE) {
                message = "argument bindings must be all-or-nothing";
            } else {
                message = "argument bindings must not mix slot numbers and names";
            }
            throw new ParseError(line, pos, message);
        }
        return kind;
    }

    private Set<String> joined(final Optional<String> tie, final int line, final int pos) {
        final Set<String> seen;
        if (tie.isPresent()) {
            if (this.labels.contains(tie.get())) {
                throw new ParseError(
                    line, pos,
                    String.format("duplicate argument binding '%s'", tie.get())
                );
            }
            seen = new HashSet<>(this.labels);
            seen.add(tie.get());
        } else {
            seen = this.labels;
        }
        return seen;
    }

    /**
     * The kinds an argument group may hold.
     *
     * @since 0.64.0
     */
    private enum Mode {

        /**
         * No argument seen yet.
         */
        UNSET,

        /**
         * Arguments carry no binding.
         */
        NONE,

        /**
         * Arguments are bound by slot number, like {@code :0}.
         */
        SLOT,

        /**
         * Arguments are bound by name, like {@code :a}.
         */
        NAME
    }
}
