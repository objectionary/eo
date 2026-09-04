/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

/**
 * One named argument of a site.
 *
 * <p>The name is what the XMIR application wrote in its {@code as}
 * attribute — a positional {@code α0} or a resolved name such as
 * {@code x} — and the shape of a record accepts either, since phino
 * resolves the positional names lazily and both spellings mean the same
 * binding.</p>
 *
 * @since 0.76.0
 */
public final class Binding {

    /**
     * The name of the binding.
     */
    private final String name;

    /**
     * The term bound to it.
     */
    private final Term term;

    /**
     * Ctor.
     * @param label The name of the binding
     * @param value The term bound to it
     */
    public Binding(final String label, final Term value) {
        this.name = label;
        this.term = value;
    }

    /**
     * The name of the binding.
     * @return The name, such as {@code α0} or {@code x}
     */
    public String label() {
        return this.name;
    }

    /**
     * The term bound to it.
     * @return The term
     */
    public Term value() {
        return this.term;
    }
}
