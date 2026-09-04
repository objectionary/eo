/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

/**
 * The attribute that copies the object and binds itself as its \rho, but
 * only when the object declares a \rho and has not been bound to one yet.
 * The terminator ({@link PhTerminator}) silently ignores this \rho itself, so no container
 * leaks into it and its cause is not masked as it propagates.
 * Every caller takes a copy of its own, since that copy is where the caller puts its
 * own arguments, and the copying happens under the monitor of this attribute, so that
 * no thread reads the object being bound while another one is still copying it. The
 * monitor is the attribute itself and not a lock of its own, because an object of the
 * language is made of these attributes and there are millions of them in the heap of a
 * program that runs for a while: a lock costs two objects each, while a monitor costs
 * nothing until two threads want it at the same time.
 * @since 0.36.0
 * @checkstyle RegexpSinglelineCheck (60 lines)
 */
@SuppressWarnings("PMD.AvoidSynchronizedStatement")
final class AtWithRho implements Attribute {

    /**
     * Original attribute.
     */
    private final Attribute original;

    /**
     * Rho.
     */
    private final Phi rho;

    /**
     * Ctor.
     * @param attr Attribute
     * @param rho Rho
     */
    AtWithRho(final Attribute attr, final Phi rho) {
        this.original = attr;
        this.rho = rho;
    }

    @Override
    public Attribute copy(final Phi self) {
        return new AtWithRho(
            this.original.copy(self),
            self
        );
    }

    @Override
    public Phi get() {
        Phi ret = this.original.get();
        if (ret.needsRho()) {
            synchronized (this) {
                ret = ret.copy();
                ret.put(Phi.RHO, this.rho);
            }
        }
        return ret;
    }

    @Override
    public void put(final Phi phi) {
        this.original.put(phi);
    }

    @Override
    public boolean vacant() {
        return this.original.vacant();
    }

    @Override
    public String φTerm() {
        return this.original.φTerm();
    }
}
