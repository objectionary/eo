/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

/**
 * Wrapper for {@link Phi}.
 *
 * @since 0.36.0
 * @checkstyle DesignForExtensionCheck (100 lines)
 */
@SuppressWarnings({"JTCOP.RuleAllTestsHaveProductionClass", "JTCOP.RuleCorrectTestName"})
abstract class PhaEnvelope implements Phi {
    /**
     * Original attribute.
     */
    private final Phi origin;

    /**
     * Ctor.
     * @param attr Attribute
     */
    PhaEnvelope(final Phi attr) {
        this.origin = attr;
    }

    @Override
    public Phi copy(final Phi self) {
        return this.origin.copy(self);
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
        return this.origin.take(name);
    }

    @Override
    public Phi take(final int pos) {
        return this.origin.take(pos);
    }

    @Override
    public void put(final int pos, final Phi object) {
        this.origin.put(pos, object);
    }

    @Override
    public void put(final String name, final Phi object) {
        this.origin.put(name, object);
    }

    @Override
    public String forma() {
        throw new UnsupportedOperationException("#forma()");
    }

    @Override
    public String locator() {
        throw new UnsupportedOperationException("#locator()");
    }

    @Override
    public byte[] delta() {
        throw new UnsupportedOperationException("#delta()");
    }
}
