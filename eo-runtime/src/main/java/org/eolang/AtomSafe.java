/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

/**
 * Atom that catches exceptions.
 *
 * <p>A {@link RuntimeException} passes through untouched, so that an
 * EO-level error keeps its own message. A JVM {@link Error} passes through
 * too, since a broken JVM is not something EO code may recover from.
 * Anything else turns into an {@link ExFailure} carrying the original as
 * its cause.</p>
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
     * Original object, expected to also be an atom.
     */
    private final Phi origin;

    /**
     * Ctor.
     *
     * @param atom Phi as atom
     */
    public AtomSafe(final Phi atom) {
        this.origin = atom;
    }

    // @checkstyle IllegalCatchCheck (14 lines)
    @Override
    @SuppressWarnings("java:S1181")
    public Phi lambda() {
        try {
            return ((Atom) this.origin).lambda();
        } catch (final InterruptedException ex) {
            Thread.currentThread().interrupt();
            throw AtomSafe.failure(ex);
        } catch (final RuntimeException | Error ex) {
            throw ex;
        } catch (final Throwable ex) {
            throw AtomSafe.failure(ex);
        }
    }

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
