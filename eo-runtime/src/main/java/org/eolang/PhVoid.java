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
    public String locator() {
        final Phi obj = this.object.get();
        if (obj == null) {
            throw new ExUnset(
                String.format(
                    "The attribute \"%s\" is not initialized, can't get locator", this.name
                )
            );
        }
        return String.format("%s:%s.∅", obj.locator(), this.name);
    }

    @Override
    public String forma() {
        return this.name;
    }

    @Override
    public void put(final String nme, final Phi phi) {
        if (!this.object.compareAndSet(null, phi)) {
            throw new ExReadOnly(
                String.format(
                    "Void attribute \"%s\" is already set, can't reset",
                    this.name
                )
            );
        }
    }

    @Override
    public void put(final int pos, final Phi phi) {
        if (!this.object.compareAndSet(null, phi)) {
            throw new ExReadOnly(
                String.format(
                    "Void attribute \"%s\" is already set, can't reset",
                    this.name
                )
            );
        }
    }

    @Override
    public Phi copy() {
        final Phi obj = this.object.get();
        if (obj == null) {
            throw new ExUnset(
                String.format("The attribute \"%s\" is not initialized, can't copy", this.name)
            );
        }
        return obj.copy();
    }

    @Override
    public boolean hasRho() {
        return false;
    }

    @Override
    public Phi take(final String nme) {
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
    public byte[] delta() {
        final Phi obj = this.object.get();
        if (obj == null) {
            throw new ExUnset(
                String.format("The attribute \"%s\" is not initialized, can't get delta", this.name)
            );
        }
        return obj.delta();
    }
}
