/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import java.util.function.Supplier;

/**
 * A method-calling object.
 * @since 0.1
 */
public final class PhDispatch extends PhOnce {

    /**
     * Ctor.
     * @param phi The object
     * @param mtd The name of method
     */
    public PhDispatch(final Phi phi, final String mtd) {
        this(
            () -> phi.take(mtd),
            () -> String.join(".", phi.φTerm(), mtd)
        );
    }

    /**
     * Ctor.
     * @param obj The object
     * @param term Supplier of the φ-term
     */
    private PhDispatch(final Supplier<Phi> obj, final Supplier<String> term) {
        super(obj, term);
    }

    @Override
    public Phi wrapped(final Supplier<Phi> obj, final Supplier<String> phrase) {
        return new PhDispatch(obj, phrase);
    }
}
