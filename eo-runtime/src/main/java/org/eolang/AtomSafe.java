/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

/**
 * Atom that catches exceptions.
 *
 * <p>A {@link RuntimeException} passes through untouched, so that an
 * EO-level error keeps its own message. Anything else turns into an
 * {@link ExFailure} carrying the original as its cause.</p>
 *
 * <p>Elsewhere we let Cactoos catch for us, with {@code ScalarWithFallback}.
 * Here we catch by hand, because {@code eo-runtime} ships with no
 * compile-scope dependencies at all.</p>
 *
 * @since 0.36.0
 */
@SuppressWarnings("PMD.AvoidCatchingGenericException")
public final class AtomSafe implements Atom {

    /**
     * Original atom.
     */
    private final Atom origin;

    /**
     * Ctor.
     * @param atom Phi as atom
     */
    public AtomSafe(final Phi atom) {
        this.origin = (Atom) atom;
    }

    // @checkstyle IllegalCatchCheck (12 lines)
    @Override
    public Phi lambda() {
        try {
            return this.origin.lambda();
        } catch (final InterruptedException ex) {
            Thread.currentThread().interrupt();
            throw AtomSafe.failure(ex);
        } catch (final RuntimeException ex) {
            throw ex;
        } catch (final Throwable ex) {
            throw AtomSafe.failure(ex);
        }
    }

    /**
     * The failure to throw out of {@link #lambda()}.
     * @param cause What the original atom threw
     * @return The failure
     */
    private static ExFailure failure(final Throwable cause) {
        return new ExFailure(
            String.format(
                "Unexpected error \"%s\" of type %s",
                cause.getMessage(),
                cause.getClass().getSimpleName()
            ),
            cause
        );
    }
}
