/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import java.util.concurrent.atomic.AtomicReference;

/**
 * Void attribute.
 *
 * <p>The attribute is not yet set, but can be set. It's writable, but
 * only once.</p>
 *
 * @since 0.1
 */
public final class AtVoid implements Attribute {

    /**
     * Name of the attribute.
     */
    private final String name;

    /**
     * Object that attribute keeps.
     */
    private final AtomicReference<Phi> object;

    /**
     * Ctor.
     * @param name The name of the attribute
     */
    public AtVoid(final String name) {
        this(name, null);
    }

    /**
     * Ctor for copying.
     * @param name Name of the attribute
     * @param phi Object
     */
    private AtVoid(final String name, final Phi phi) {
        this.name = name;
        this.object = new AtomicReference<>(phi);
    }

    @Override
    public Attribute copy(final Phi self) {
        final Phi obj = this.object.get();
        final Phi copy;
        if (obj == null) {
            copy = null;
        } else {
            copy = obj.copy();
        }
        return new AtVoid(this.name, copy);
    }

    @Override
    public Phi get() {
        final Phi phi = this.object.get();
        final Phi result;
        if (phi == null) {
            result = new PhTerminator();
        } else {
            result = phi;
        }
        return result;
    }

    @Override
    public void put(final Phi phi) {
        if (!this.object.compareAndSet(null, phi)) {
            throw new ExReadOnly(
                String.format(
                    "This void attribute \"%s\" is already set, can't reset",
                    this.name
                )
            );
        }
    }

    @Override
    public String φTerm() {
        final Phi phi = this.object.get();
        final String term;
        if (phi == null) {
            term = "?";
        } else {
            term = phi.φTerm();
        }
        return term;
    }
}
