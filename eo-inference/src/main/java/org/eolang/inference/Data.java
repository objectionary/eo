/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import org.xembly.Directives;

/**
 * An object that is a datum.
 *
 * <p>The bytes of a literal are the ground the whole program stands on, so
 * this carries nothing: there is nothing to know about {@code 01-} beyond
 * that it is what it is, and the bytes themselves are in the XMIR the row
 * points at.</p>
 *
 * @since 0.69.0
 */
final class Data implements Type {

    @Override
    public Directives directives() {
        return new Directives().add("data").up();
    }
}
