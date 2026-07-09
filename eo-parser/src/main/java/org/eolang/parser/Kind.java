/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

/**
 * Outer kind of an EO expression — Appendix A of the spec.
 *
 * <p>The outer kind is the kind of an expression's outermost AST node;
 * because the parser is AST-free, it is carried on the indent-stack
 * {@link Level} instead. The kind can change as more lines arrive at
 * deeper indents (e.g., a {@code HEAD} promotes to {@code VAPPLICATION}
 * once the first deeper child appears). The same kind progresses through
 * {@link Openness} states; the kind name itself never regresses.</p>
 *
 * <p>The three kinds in the horizontally-completed set
 * ({@link #HAPPLICATION}, {@link #REVERSED_WITH_HARGS},
 * {@link #VMETHOD_WITH_HARGS}) never receive deeper children and cannot
 * be wrapped by a same-indent {@code .method}.
 * {@link #horizontallyCompleted()} is the single source of truth for
 * that set; R-5.2.3 and R-6.1.1 read it. An {@link #ONLY_PHI_FORMATION}
 * with a bare (zero-hargs) φ is instead {@link Openness#OPEN}: its φ
 * accepts deeper-indent vertical arguments (§4.5).</p>
 *
 * <p>The {@link #TOP_LEVEL} sentinel is not a real expression kind — it is
 * the {@code parent_kind} for entries pushed at indent 0 (R-5.2.11), used
 * by close-time checks to recognise top-level naming requirements
 * (R-5.3.1). *
 *
 * @since 0.1
 */
@SuppressWarnings("PMD.LongVariable")
enum Kind {

    /**
     * Sentinel: parent of an indent-0 expression. Not a real kind.
     */
    TOP_LEVEL,

    /**
     * Bare identifier / literal / paren group, no chain, no horizontal args.
     */
    HEAD,

    /**
     * Chained dispatch {@code x.y.z} with zero horizontal args.
     */
    HMETHOD,

    /**
     * Head with one or more horizontal args. Horizontally completed.
     */
    HAPPLICATION,

    /**
     * {@code [params] [> name]}. Body may follow.
     */
    BARE_FORMATION,

    /**
     * Reversed dispatch in vertical form: {@code name.} with deeper-indent
     * receiver + args.
     */
    BARE_REVERSED,

    /**
     * Reversed dispatch with horizontal args: {@code name. arg1 arg2}.
     * Horizontally completed.
     */
    REVERSED_WITH_HARGS,

    /**
     * Compact tuple {@code head *N}.
     */
    COMPACT_TUPLE,

    /**
     * Only-phi formation {@code expr > [params] > name}. Horizontally
     * completed when the φ (its {@code expr}) carries horizontal args;
     * otherwise open, so its φ accepts deeper-indent vertical arguments.
     */
    ONLY_PHI_FORMATION,

    /**
     * Multi-line vertical application: head + deeper-indent argument block.
     */
    VAPPLICATION,

    /**
     * Multi-line vertical method chain (same-indent {@code .method}
     * continuations on a head).
     */
    VMETHOD,

    /**
     * Vertical method chain whose last link carries horizontal args.
     * Horizontally completed.
     */
    VMETHOD_WITH_HARGS,

    /**
     * Triple-quoted {@code """…"""} text block.
     */
    TEXT_BLOCK,

    /**
     * Vertical void attribute {@code ? > name} (R-3.4.7). A closed leaf
     * that must precede every non-void child of its formation.
     */
    VOID;

    /**
     * Whether this kind is in the horizontally-completed set.
     *
     * <p>Members of this set cannot be extended by deeper-indent children
     * nor wrapped by a same-indent {@code .method}. Single source of truth
     * for R-5.2.3 and R-6.1.1.</p>
     *
     * @return True iff this kind is horizontally completed
     */
    boolean horizontallyCompleted() {
        return this == HAPPLICATION
            || this == REVERSED_WITH_HARGS
            || this == VMETHOD_WITH_HARGS;
    }

    /**
     * Whether this kind opens a formation — a fresh naming scope whose
     * direct children are named attributes rather than positional
     * arguments.
     *
     * <p>Read by {@link Stack} to stop the only-phi argument-position
     * flag at a nested formation boundary: inside a formation, naming
     * resumes as usual (§6.2), so the no-name-on-argument rule of §4.5
     * must not leak past it.</p>
     *
     * @return True iff this kind is a formation
     */
    boolean formation() {
        return this == BARE_FORMATION || this == ONLY_PHI_FORMATION;
    }
}
