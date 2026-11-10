/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import org.xembly.Directives;

/**
 * An object that is a copy of another one, as the text states it.
 *
 * <p>{@link Starts} gives one to every bare head: a bare {@code Φ} is a
 * copy of the global object, and a bare {@code ξ} is a copy of the formation
 * around it. The row is a {@code ref} element with that locator and nothing
 * inside, and {@link FactsStream} reads it back as a {@link Head}. What a
 * call puts into the voids of a copy is written by {@link Applications},
 * into the {@code calls} section.</p>
 *
 * @since 0.69.0
 */
final class Ref implements Type {

    /**
     * The locator of what this object is a copy of.
     */
    private final String loc;

    /**
     * Ctor.
     *
     * @param target The locator of what this object is a copy of
     */
    Ref(final String target) {
        this.loc = target;
    }

    @Override
    public String names() {
        return this.loc;
    }

    @Override
    public Directives directives() {
        return new Directives().add("ref").attr("loc", this.loc).up();
    }
}
