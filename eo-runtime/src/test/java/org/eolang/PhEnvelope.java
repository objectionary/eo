/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
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
abstract class PhEnvelope implements Phi {
    /**
     * Original attribute.
     */
    private final Phi origin;

    /**
     * Ctor.
     * @param attr Attribute
     */
    PhEnvelope(final Phi attr) {
        this.origin = attr;
    }

    @Override
    public Phi copy(final Phi self) {
        return this.origin.copy(self);
    }

    @Override
    public Phi copy() {
        return this.origin.copy();
    }

    @Override
    public boolean hasRho() {
        return this.origin.hasRho();
    }

    @Override
    public Phi take(final String name) {
        return this.origin.take(name);
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
        return this.origin.forma();
    }

    @Override
    public String locator() {
        return this.origin.locator();
    }

    @Override
    public byte[] delta() {
        return this.origin.delta();
    }
}
