/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import org.xembly.Directives;

/**
 * An object we looked at and could say nothing about.
 *
 * <p>Not the same as an object nobody looked at, which is what an absent row
 * used to mean as well. A reader that cannot tell the two apart has to guess
 * whether the tables gave up or never tried, and a checker that guesses is
 * worse than one that stays undecided.</p>
 *
 * @since 0.69.0
 */
final class Unknown implements Type {

    @Override
    public Directives directives() {
        return new Directives().add("unknown").up();
    }
}
