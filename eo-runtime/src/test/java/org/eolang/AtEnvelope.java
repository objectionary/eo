/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

/**
 * Wrapper for {@link Attr}.
 *
 * @since 0.36.0
 * @checkstyle DesignForExtensionCheck (100 lines)
 */
@SuppressWarnings({"JTCOP.RuleAllTestsHaveProductionClass", "JTCOP.RuleCorrectTestName"})
abstract class AtEnvelope implements Attr {
    /**
     * Original attribute.
     */
    private final Attr origin;

    /**
     * Ctor.
     * @param attr Attribute
     */
    AtEnvelope(final Attr attr) {
        this.origin = attr;
    }

    @Override
    public Attr copy(final Phi self) {
        return this.origin.copy(self);
    }

    @Override
    public Phi get() {
        return this.origin.get();
    }

    @Override
    public void put(final Phi phi) {
        this.origin.put(phi);
    }
}
