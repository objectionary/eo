/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import java.util.Optional;
import java.util.function.Supplier;

/**
 * A method-calling object.
 *
 * @since 0.1
 */
public final class PhDispatch extends PhOnce {

    /**
     * Ctor.
     *
     * @param phi The object
     * @param mtd The name of method
     */
    public PhDispatch(final Phi phi, final String mtd) {
        this(
            () -> phi.take(mtd),
            Optional.of(() -> String.join(".", phi.φTerm(), mtd))
        );
    }

    /**
     * Ctor.
     *
     * @param obj The object
     * @param phrase Supplier of the φ-term
     */
    private PhDispatch(final Supplier<Phi> obj, final Optional<Supplier<String>> phrase) {
        super(obj, phrase);
    }

    @Override
    public Phi wrapped(final Supplier<Phi> obj, final Optional<Supplier<String>> phrase) {
        return new PhDispatch(obj, phrase);
    }
}
