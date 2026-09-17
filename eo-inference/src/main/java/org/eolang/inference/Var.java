/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import org.xembly.Directives;

/**
 * A variable: an object whose type only a caller can decide.
 *
 * <p>A void is one. What it holds is put there by whoever copies the object
 * that keeps it, so the text of the program says nothing about what it is —
 * which is a fact, and a different one from the silence around an object
 * nobody looked at.</p>
 *
 * <p>In the row of the void itself the variable needs no name: the row is
 * keyed by the locator, and saying the same locator twice would only invite
 * the two to disagree. Written anywhere else — inside a choice of what a void
 * is ever filled with, say — it carries the locator, since there is nothing
 * else there to say which variable it is.</p>
 *
 * @since 0.69.0
 */
final class Var implements Type {

    /**
     * The locator of the void, empty when the place it is written says it.
     */
    private final String name;

    /**
     * Ctor.
     */
    Var() {
        this("");
    }

    /**
     * Ctor.
     *
     * @param id The locator of the void
     */
    Var(final String id) {
        this.name = id;
    }

    @Override
    public Directives directives() {
        final Directives dirs = new Directives().add("var");
        if (!this.name.isEmpty()) {
            dirs.attr("id", this.name);
        }
        return dirs.up();
    }
}
