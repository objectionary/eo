/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

/**
 * Openness of an indent-stack entry — §1.6 of the spec.
 *
 * <p>Independent dimension from {@link Kind}: the same kind progresses
 * through openness states as more lines arrive at deeper indents or as
 * same-indent {@code .method} continuations land. Three values:</p>
 *
 * <ul>
 *   <li>{@link #OPEN} — may still receive deeper-indent children or a
 *   same-indent {@code .method} continuation.</li>
 *   <li>{@link #VERTICAL_COMPLETED} — child block has ended; a same-indent
 *   {@code .method} may still wrap it, but no more vertical args may be
 *   added.</li>
 *   <li>{@link #HORIZONTAL_COMPLETED} — cannot be extended in any
 *   direction. {@link Kind#horizontallyCompleted()} pins which kinds
 *   start out in this state.</li>
 * </ul>
 *
 * @since 0.1
 */
@SuppressWarnings("PMD.LongVariable")
enum Openness {

    /**
     * May accept deeper children and same-indent {@code .method}.
     */
    OPEN,

    /**
     * Child block has ended; same-indent {@code .method} may wrap.
     */
    VERTICAL_COMPLETED,

    /**
     * Closed in every direction.
     */
    HORIZONTAL_COMPLETED
}
