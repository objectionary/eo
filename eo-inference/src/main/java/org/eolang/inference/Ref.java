/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import org.xembly.Directives;

/**
 * The locator a bare head refers to in the facts.
 *
 * <p>{@link Starts} uses this for a bare {@code Φ} or {@code ξ}, whose
 * target is known from the source. It writes a {@code ref} with that
 * locator into the {@code heads} section of {@code facts.xml}. Calls and
 * their arguments are recorded separately by {@link Applications}.</p>
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
