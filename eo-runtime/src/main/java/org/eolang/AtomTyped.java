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
 * is raised. An empty or generic (type variable {@code A}–{@code F}) declared
 * type leaves the computed object unchecked.</p>
 *
 * <p>The check is <em>opt-in</em>, controlled by the {@code eo.typing} system
 * property (default {@code false}). It is off by default because an atom with
 * an error-branch returns a union — the declared forma <em>or</em> a caller
 * fallback / ⊥ — that a single declared forma cannot express, so the strict
 * check would reject valid fallbacks. Enable it (e.g. {@code -Deo.typing=true})
 * to verify atom return types where no error-branches are used.</p>
 *
 * @since 0.57
 */
public final class AtomTyped implements Atom {

    /**
     * Whether atom return-type checking is enabled, read from the
     * {@code eo.typing} system property (default {@code false}).
     */
    private static final boolean ENABLED = Boolean.getBoolean("eo.typing");

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
        if (AtomTyped.ENABLED
            && !this.declared.isEmpty()
            && !AtomTyped.generic(this.declared)
            && !this.declared.equals(computed.forma())) {
            throw new ExFailure(
                "The atom computed an object of type \"%s\", but \"%s\" was declared",
                computed.forma(),
                this.declared
            );
        }
        return computed;
    }

    /**
     * Whether the declared type is a generic type variable — a single
     * letter {@code A}–{@code F} (§3.10.10). A generic return depends on
     * the atom's arguments, so a plain forma-equality check cannot verify
     * it; such an atom is left unchecked, exactly like one that declares
     * no type.
     * @param declared The declared type
     * @return True when the declared type is a generic type variable
     */
    private static boolean generic(final String declared) {
        return declared.matches("[A-F]");
    }
}
