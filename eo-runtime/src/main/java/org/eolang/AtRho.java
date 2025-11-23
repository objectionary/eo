/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import java.util.concurrent.atomic.AtomicReference;

/**
 * Special attribute for \rho.
 *
 * <p>The attribute can be set only once, and it ignores all other puts.</p>
 *
 * @since 0.36.0
 */
final class AtRho implements Attr {
    /**
     * Rho.
     */
    private final AtomicReference<Phi> rho;

    /**
     * Ctor.
     */
    AtRho() {
        this(null);
    }

    /**
     * Ctor.
     * @param rho Rho.
     */
    private AtRho(final Phi rho) {
        this.rho = new AtomicReference<>(rho);
    }

    @Override
    public Attr copy(final Phi self) {
        return new AtRho(this.rho.get());
    }

    @Override
    public Phi get() {
        if (this.rho.get() == null) {
            throw new ExUnset(
                String.format("The \"%s\" attribute is not set", Attr.RHO)
            );
        }
        return this.rho.get();
    }

    @Override
    public void put(final Phi phi) {
        if (this.rho.get() == null) {
            this.rho.set(phi);
        }
    }
}
