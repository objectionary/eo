/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

/**
 * Atom that catches exceptions.
 *
 * @since 0.36.0
 */
@SuppressWarnings({"PMD.AvoidCatchingGenericException", "PMD.AvoidRethrowingException"})
public final class AtomSafe implements Atom {
    /**
     * Original atom.
     */
    private final Atom origin;

    /**
     * Ctor.
     * @param atom Phi as atom.
     */
    public AtomSafe(final Phi atom) {
        this.origin = (Atom) atom;
    }

    @Override
    public Phi lambda() {
        try {
            return this.origin.lambda();
        } catch (final InterruptedException ex) {
            Thread.currentThread().interrupt();
            throw new ExInterrupted(ex);
            // @checkstyle IllegalCatchCheck (3 line)
        } catch (final RuntimeException ex) {
            throw ex;
        } catch (final Exception ex) {
            throw new ExFailure(
                String.format(
                    "Unexpected error \"%s\" of type %s",
                    ex.getMessage(),
                    ex.getClass().getSimpleName()
                ),
                ex
            );
        }
    }
}
