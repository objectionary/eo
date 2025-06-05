/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

/**
 * Default attribute that just keeps single object.
 * Used for tests mostly.
 *
 * <p>The class is NOT thread-safe.</p>
 *
 * @since 0.1
 */
@SuppressWarnings({
    "JTCOP.RuleAllTestsHaveProductionClass",
    "JTCOP.RuleCorrectTestName",
    "JTCOP.RuleInheritanceInTests"
})
final class PhSimple extends PhEnvelope {
    /**
     * Ctor.
     */
    PhSimple() {
        this(Phi.Φ);
    }

    /**
     * Ctor.
     * @param object Object that attribute keeps
     */
    PhSimple(final Phi object) {
        super(new PhComposite(object, arg -> object));
    }
}
