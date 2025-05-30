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
public final class PhVoid implements Phi {
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
    public PhVoid(final String name) {
        this(name, null);
    }

    /**
     * Ctor for copying.
     * @param name Name of the attribute
     * @param phi Object
     */
    private PhVoid(final String name, final Phi phi) {
        this.name = name;
        this.object = new AtomicReference<>(phi);
    }

    @Override
    public Phi copy(final Phi self) {
        final Phi obj = this.object.get();
        final Phi copy;
        if (obj == null) {
            copy = null;
        } else {
            copy = obj.copy();
        }
        return new PhVoid(this.name, copy);
    }

    @Override
    public void put(final String nme, final Phi phi) {
        this.put(phi);
    }

    @Override
    public String locator() {
        return String.format("%s:%s.∅", this.object.get().locator(), this.name);
    }

    @Override
    public String forma() {
        return this.name;
    }

    @Override
    public void put(final int pos, final Phi phi) {
        this.put(phi);
    }

    @Override
    public Phi take(final int pos) {
        return this.get();
    }

    @Override
    public Phi copy() {
        return this.object.get().copy();
    }

    @Override
    public boolean hasRho() {
        return false;
    }

    @Override
    public Phi take(final String nme) {
        return this.get();
    }

    @Override
    public byte[] delta() {
        return this.object.get().delta();
    }

    private void put(final Phi phi) {
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

    private Phi get() {
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
}
