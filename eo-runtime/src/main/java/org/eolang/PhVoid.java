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
     *
     * @param name The name of the attribute
     */
    public PhVoid(final String name) {
        this(name, null);
    }

    /**
     * Ctor for copying.
     *
     * @param name Name of the attribute
     * @param phi  Object
     */
    private PhVoid(final String name, final Phi phi) {
        this.name = name;
        this.object = new AtomicReference<>(phi);
    }

    /**
     * Throws {@link ExUnset} if object is not set
     *
     * @param cant Message
     */
    private void validate(final String cant) {
        if (this.object.get() == null) {
            throw new ExUnset(
                String.format(
                    "The attribute \"%s\" is not initialized, can't %s", cant, this.name
                )
            );
        }
    }

    /**
     * Set void attribute.
     * @param phi Object to set
     */
    public void set(final Phi phi) {
        final boolean set = this.object.compareAndSet(null, phi);
        if (!set && !this.name.equals(Phi.RHO)) {
            throw new ExFailure(
                String.format(
                    "The attribute \"%s\" is already set, can't reset", this.name
                )
            );
        }
    }

    /**
     * Returns True if void attribute is set.
     * @return True if set
     */
    public boolean isSet() {
        return this.object.get() != null;
    }

    @Override
    public Phi copy(final Phi self) {
        final Phi obj = this.object.get();
        final Phi copy;
        if (obj == null) {
            copy = null;
        } else {
            copy = obj.copy(self);
        }
        return new PhVoid(this.name, copy);
    }

    @Override
    public Phi copy() {
        throw new ExFailure("Should never be called");
    }

    @Override
    public Phi take(final int pos) {
        throw new ExFailure("Method to be removed");
    }

    @Override
    public Phi take(final String nme) {
        this.validate("take(String)");
        return this.object.get().take(nme);
    }

    @Override
    public void put(final String nme, final Phi phi) {
        this.validate("put(String)");
        this.object.get().put(nme, phi);
    }

    @Override
    public void put(final int pos, final Phi phi) {
        this.validate("put(int)");
        this.object.get().put(pos, phi);
    }

    @Override
    public String locator() {
        this.validate("get locator");
        return String.format("%s:%s.∅", this.object.get().locator(), this.name);
    }

    @Override
    public String forma() {
        return this.name;
    }

    @Override
    public boolean hasRho() {
        final boolean has;
        if (this.object.get() == null) {
            has = false;
        } else {
            has = this.object.get().hasRho();
        }
        return has;
    }

    @Override
    public byte[] delta() {
        this.validate("take data");
        return this.object.get().delta();
    }
}
