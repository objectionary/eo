/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

/**
 * The attribute that tries to copy object and set \rho to it if it has not already set.
 * @since 0.36.0
 */
final class AtWithRho implements Attr {
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
    AtWithRho(final Attr attr, final Phi rho) {
        this.original = attr;
        this.rho = rho;
    }

    @Override
    public Attr copy(final Phi self) {
        return new AtWithRho(
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
}
