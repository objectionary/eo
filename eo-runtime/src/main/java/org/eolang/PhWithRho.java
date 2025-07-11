/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

/**
 * A decorator for an attribute that ensures the object has a {@code \rho} attribute.
 * If not already set, it sets it.
 *
 * @since 0.36.0
 */
@SuppressWarnings("PMD.TooManyMethods")
final class PhWithRho extends AbstractPhWithAttr {

    /**
     * Rho.
     */
    private final Phi rho;

    /**
     * Ctor.
     * @param attr Attribute
     * @param rho Rho
     */
    PhWithRho(final Phi attr, final Phi rho) {
        super(attr);
        this.rho = rho;
    }

    @Override
    public Phi copy() {
        return new PhWithRho(origin().copy(), this.rho);
    }

    @Override
    public boolean hasRho() {
        return origin().hasRho();
    }

    @Override
    public Phi take(final String name) {
        Phi ret = origin().take(name);
        if (!ret.hasRho()) {
            ret = ret.copy();
            ret.put(Phi.RHO, this.rho);
        }
        return ret;
    }

    @Override
    public Phi take(final int pos) {
        Phi ret = origin().take(pos);
        if (!ret.hasRho()) {
            ret = ret.copy();
            ret.put(Phi.RHO, this.rho);
        }
        return ret;
    }

    @Override
    public void put(final int pos, final Phi object) {
        origin().put(pos, object);
    }

    @Override
    public void put(final String name, final Phi object) {
        origin().put(name, object);
    }

    @Override
    public String locator() {
        return origin().locator();
    }

    @Override
    public String forma() {
        return origin().forma();
    }

    @Override
    public Phi copy(final Phi self) {
        return new PhWithRho(
            origin().copy(self),
            self
        );
    }

    @Override
    public byte[] delta() {
        return origin().delta();
    }
}
