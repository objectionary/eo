/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import java.util.Optional;
import java.util.function.Supplier;

/**
 * An object that is built only when something first asks it for anything.
 *
 * <p>A Java atom made by eo-lowering passes it as an argument of a call,
 * because EO computes an argument only when the object that gets it reads
 * it, and some objects never do: {@code false.and x} never looks at
 * {@code x}. An argument built in advance would run code that EO never
 * runs, and that code may fail.</p>
 *
 * @since 0.78.0
 */
public final class PhLazy extends PhOnce {

    /**
     * Ctor.
     *
     * @param sup Supplier of the object
     */
    public PhLazy(final Supplier<Phi> sup) {
        this(sup, Optional.empty());
    }

    /**
     * Ctor.
     *
     * @param sup Supplier of the object
     * @param phrase Supplier of the φ-term
     */
    private PhLazy(final Supplier<Phi> sup, final Optional<Supplier<String>> phrase) {
        super(sup, phrase);
    }

    @Override
    public Phi wrapped(final Supplier<Phi> sup, final Optional<Supplier<String>> phrase) {
        return new PhLazy(sup, phrase);
    }
}
