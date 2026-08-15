/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import org.xembly.Directives;

/**
 * An object that never comes back with a value.
 *
 * <p>A termination is a complete answer and not a missing one, which is the
 * whole of why it is written down: an expression that terminates fits
 * wherever it is put, and an expression nothing is known about fits nowhere
 * in particular. Leaving the row out says the second about the first.</p>
 *
 * <p>Whatever the termination was caused by is an object of its own with a
 * row of its own, so nothing of it is carried here.</p>
 *
 * @since 0.69.0
 */
final class Bottom implements Type {

    @Override
    public Directives directives() {
        return new Directives().add("bottom").up();
    }
}
