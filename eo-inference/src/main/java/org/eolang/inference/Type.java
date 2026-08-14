/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import org.xembly.Directives;

/**
 * What an object of a program turns out to be.
 *
 * <p>There are several kinds of answer and there will be more: an object is
 * a copy of another one, or a datum, or a termination, or a choice between
 * several objects. They have nothing in common except where they are written
 * — inside the row of the object they are about — so that is all this says,
 * and a new kind is a new class rather than another column of a table nobody
 * can keep straight.</p>
 *
 * <p>What comes back leaves the cursor where it found it, since a row holds
 * one type and the row that follows starts beside it.</p>
 *
 * @since 0.69.0
 */
@FunctionalInterface
interface Type {

    /**
     * This type as the contents of a row.
     * @return The directives
     */
    Directives directives();
}
