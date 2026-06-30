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
public final class PhTerminator implements Phi {

    @Override
    public Phi copy() {
        throw PhTerminator.fail();
    }

    @Override
    public boolean hasRho() {
        throw PhTerminator.fail();
    }

    @Override
    public Phi take(final String name) {
        throw PhTerminator.fail();
    }

    @Override
    public void put(final int pos, final Phi object) {
        throw PhTerminator.fail();
    }

    @Override
    public void put(final String name, final Phi object) {
        throw PhTerminator.fail();
    }

    @Override
    public String locator() {
        throw PhTerminator.fail();
    }

    @Override
    public String forma() {
        throw PhTerminator.fail();
    }

    @Override
    public byte[] delta() {
        throw PhTerminator.fail();
    }

    @Override
    public String φTerm() {
        throw PhTerminator.fail();
    }

    /**
     * The failure raised by every interaction with ⊥.
     * @return The exception to throw
     */
    private static ExFailure fail() {
        return new ExFailure("the ⊥ object is a terminated computation and cannot be used");
    }
}
