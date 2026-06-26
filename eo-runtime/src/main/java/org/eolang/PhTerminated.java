/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

/**
 * The ⊥ ("bottom") object of φ-calculus — a terminated computation.
 *
 * <p>It has no data, no attributes, and no behaviour: every interaction
 * with it aborts the program through an {@link ExFailure}. Because EO
 * {@code try} only intercepts {@link EOerror.ExError}, this failure
 * cannot be caught, so touching ⊥ terminates the program for good.</p>
 *
 * @since 0.73.1
 */
public final class PhTerminated implements Phi {

    /**
     * The message of the failure raised by every interaction with ⊥.
     */
    private static final String MESSAGE =
        "the ⊥ object is a terminated computation and cannot be used";

    @Override
    public Phi copy() {
        throw new ExFailure(PhTerminated.MESSAGE);
    }

    @Override
    public boolean hasRho() {
        throw new ExFailure(PhTerminated.MESSAGE);
    }

    @Override
    public Phi take(final String name) {
        throw new ExFailure(PhTerminated.MESSAGE);
    }

    @Override
    public void put(final int pos, final Phi object) {
        throw new ExFailure(PhTerminated.MESSAGE);
    }

    @Override
    public void put(final String name, final Phi object) {
        throw new ExFailure(PhTerminated.MESSAGE);
    }

    @Override
    public String locator() {
        throw new ExFailure(PhTerminated.MESSAGE);
    }

    @Override
    public String forma() {
        throw new ExFailure(PhTerminated.MESSAGE);
    }

    @Override
    public byte[] delta() {
        throw new ExFailure(PhTerminated.MESSAGE);
    }

    @Override
    public String φTerm() {
        throw new ExFailure(PhTerminated.MESSAGE);
    }
}
