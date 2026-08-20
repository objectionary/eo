/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import java.util.Objects;
import java.util.concurrent.atomic.AtomicReference;

/**
 * The attribute that holds the receiver.
 *
 * <p>It behaves as a void in every respect but one. A void owns what
 * fills it, so copying the object copies the value too; a receiver
 * points the other way, at the object this one hangs off, and
 * duplicating that on every copy would clone the whole chain above.
 * So {@link #copy(Phi)} carries the reference over untouched.</p>
 *
 * @since 0.36.0
 */
public final class AtRho implements Attribute {

    /**
     * Rho.
     */
    private final AtomicReference<Phi> rho;

    /**
     * Ctor.
     */
    public AtRho() {
        this(null);
    }

    /**
     * Ctor.
     * @param rho Rho
     */
    private AtRho(final Phi rho) {
        this.rho = new AtomicReference<>(rho);
    }

    @Override
    public Attribute copy(final Phi self) {
        return new AtRho(this.rho.get());
    }

    @Override
    public Phi get() {
        final Phi phi = this.rho.get();
        final Phi result;
        if (phi == null) {
            result = new PhTerminator(
                String.format("the attribute \"%s\" is not set", Phi.RHO)
            );
        } else {
            result = phi;
        }
        return result;
    }

    @Override
    public void put(final Phi phi) {
        Objects.requireNonNull(phi, "Attribute value can't be null");
        if (!this.rho.compareAndSet(null, phi)) {
            throw new ExReadOnly(
                String.format(
                    "This void attribute \"%s\" is already set, can't reset",
                    Phi.RHO
                )
            );
        }
    }

    @Override
    public boolean vacant() {
        return this.rho.get() == null;
    }

    @Override
    public String φTerm() {
        final String term;
        if (this.rho.get() == null) {
            term = "?";
        } else {
            term = "^";
        }
        return term;
    }
}
