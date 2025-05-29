/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

/**
 * The attribute that tries to copy object and set \rho to it if it has not already set.
 * @since 0.36.0
 */
final class PhWithRho implements Phi {
    /**
     * Original attribute.
     */
    private final Attr original;

    /**
     * Rho.
     */
    private final Phi rho;

    /**
     * Ctor.
     * @param attr Attribute
     * @param rho Rho
     */
    PhWithRho(final Attr attr, final Phi rho) {
        this.original = attr;
        this.rho = rho;
    }

    @Override
    public Phi copy() {
        throw new UnsupportedOperationException("#copy()");
    }

    @Override
    public boolean hasRho() {
        throw new UnsupportedOperationException("#hasRho()");
    }

    @Override
    public Phi take(final String name) {
        throw new UnsupportedOperationException("#take()");
    }

    @Override
    public Phi take(final int pos) {
        throw new UnsupportedOperationException("#take()");
    }

    @Override
    public void put(final int pos, final Phi object) {
        this.original.put(object);
    }

    @Override
    public void put(final String name, final Phi object) {
        this.original.put(object);
    }

    @Override
    public String locator() {
        throw new UnsupportedOperationException("#locator()");
    }

    @Override
    public String forma() {
        throw new UnsupportedOperationException("#forma()");
    }

    @Override
    public Phi copy(final Phi self) {
        return new PhWithRho(
            this.original.copy(self),
            self
        );
    }

    @Override
    public Phi get() {
        Phi ret = this.original.get();
        if (!ret.hasRho()) {
            ret = ret.copy();
            ret.put(Attr.RHO, this.rho);
        }
        return ret;
    }

    @Override
    public void put(final Phi phi) {
        this.original.put(phi);
    }

    /**
     * Returns the original attribute.
     * @return The original attribute
     */
    Attr origin() {
        return this.original;
    }

    @Override
    public byte[] delta() {
        throw new UnsupportedOperationException("#delta()");
    }
}
