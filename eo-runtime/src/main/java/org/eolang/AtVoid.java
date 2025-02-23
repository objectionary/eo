/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
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
public final class AtVoid implements Attr {
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
    public Attr copy(final Phi self) {
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
        if (phi == null) {
            throw new ExUnset(
                String.format(
                    "The attribute \"%s\" is not initialized, can't read", this.name
                )
            );
        }
        return phi;
    }

    @Override
    public void put(final Phi phi) {
        if (this.object.get() == null) {
            this.object.set(phi);
        } else {
            throw new ExReadOnly(
                String.format(
                    "This void attribute \"%s\" is already set, can't reset",
                    this.name
                )
            );
        }
    }
}
