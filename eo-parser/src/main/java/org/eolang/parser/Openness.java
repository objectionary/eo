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
 * <li>{@link #OPEN} — may still receive deeper-indent children or a
 * same-indent {@code .method} continuation.</li>
 * <li>{@link #VCOMPLETED} — vertically completed: the child block has
 * ended; a same-indent {@code .method} may still wrap it, but no more
 * vertical args may be added.</li>
 * <li>{@link #HCOMPLETED} — horizontally completed: cannot be extended
 * in any direction.</li>
 * </ul>
 *
 * @since 0.1
 */
enum Openness {

    /**
     * May accept deeper children and same-indent {@code .method}.
     */
    OPEN,

    /**
     * Vertically completed: child block has ended; same-indent
     * {@code .method} may wrap.
     */
    VCOMPLETED,

    /**
     * Horizontally completed: closed in every direction.
     */
    HCOMPLETED
}
