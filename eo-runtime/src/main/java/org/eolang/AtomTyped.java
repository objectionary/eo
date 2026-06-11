/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

/**
 * Atom that verifies the type of the object it computes.
 *
 * <p>It dispatches the wrapped atom's lambda safely (through {@link AtomSafe})
 * and then checks that the {@code forma} of the computed object equals the type
 * declared for the atom in EO (the {@code /Q.foo} return-type suffix). A
 * mismatch means the atom broke its declared contract, so an {@link ExFailure}
 * is raised. An empty declared type leaves the computed object unchecked.</p>
 *
 * @since 0.57
 */
public final class AtomTyped implements Atom {

    /**
     * The atom to dispatch.
     */
    private final Phi origin;

    /**
     * The forma declared for the computed object.
     */
    private final String declared;

    /**
     * Ctor.
     * @param atom The atom to dispatch
     * @param forma The declared forma
     */
    public AtomTyped(final Phi atom, final String forma) {
        this.origin = atom;
        this.declared = forma;
    }

    @Override
    public Phi lambda() {
        final Phi computed = new AtomSafe(this.origin).lambda();
        if (!this.declared.isEmpty() && !this.declared.equals(computed.forma())) {
            throw new ExFailure(
                "The atom computed an object of type \"%s\", but \"%s\" was declared",
                computed.forma(),
                this.declared
            );
        }
        return computed;
    }
}
