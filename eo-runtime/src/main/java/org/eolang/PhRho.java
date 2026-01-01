/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import java.util.concurrent.atomic.AtomicReference;

/**
 * Phi representation of \rho attribute.
 *
 * <p>The attribute can be set only once, and it ignores all other puts.</p>
 *
 * @since 0.36.0
 */
final class PhRho implements Phi {
    /**
     * Rho.
     */
    private final AtomicReference<Phi> rho;

    /**
     * Ctor.
     */
    PhRho() {
        this(null);
    }

    /**
     * Ctor.
     * @param rho Rho.
     */
    private PhRho(final Phi rho) {
        this.rho = new AtomicReference<>(rho);
    }

    @Override
    public Phi copy() {
        return new PhRho(this.rho.get());
    }

    @Override
    public boolean hasRho() {
        return this.rho.get().hasRho();
    }

    @Override
    public Phi take(final String name) {
        if (this.rho.get() == null) {
            throw new ExUnset(
                String.format("The \"%s\" attribute is not set", Phi.RHO)
            );
        }
        return this.rho.get();
    }

    @Override
    public void put(final int pos, final Phi object) {
        if (this.rho.get() == null) {
            this.rho.set(object);
        }
    }

    @Override
    public void put(final String name, final Phi object) {
        if (this.rho.get() == null) {
            this.rho.set(object);
        }
    }

    @Override
    public String locator() {
        return this.rho.get().locator();
    }

    @Override
    public String forma() {
        return this.rho.get().forma();
    }

    @Override
    public Phi copy(final Phi self) {
        return new PhRho(this.rho.get());
    }

    @Override
    public byte[] delta() {
        return this.rho.get().delta();
    }
}
