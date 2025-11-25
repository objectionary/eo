/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

/**
 * The attribute that tries to copy object and set \rho to it if it has not already set.
 * This attribute is NOT thread safe!
 *
 * @since 0.36.0
 * @todo #4673:30min Replace all PhWithRho occurrences with AtWithRho when all other
 *  Attr classes are returned to eo-runtime.
 *  This class does not do anything useful right now, it is not used anywhere.
 *  The class is added as part of the task where Attr and its classes are returned
 *  to eo-runtime. When all other classes are returned - this class must replace
 *  PhWithRho everywhere.
 * @todo #4673:30min The {@link AtWithRho#get()} is not thread safe. If multiple threads
 *  call get() concurrently when the underlying object lacks RHO, each thread will:
 *  1. Pass the !ret.hasRho() check
 *  2. Create its own copy via ret.copy()
 *  3. Attempt to set RHO on its copy
 *  This results in different threads receiving different copies, violating the expectation
 *  that get() returns a consistent view of the attribute's value.
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
            ret.put(Phi.RHO, this.rho);
        }
        return ret;
    }

    @Override
    public void put(final Phi phi) {
        this.original.put(phi);
    }
}
