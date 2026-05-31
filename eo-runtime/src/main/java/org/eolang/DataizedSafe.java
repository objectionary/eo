/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import java.util.function.Supplier;

/**
 * Safe dataization helpers for {@code ?.} navigation (issue #5166).
 *
 * <p>These methods catch {@link EOerror.ExError} and return the error
 * object as a {@link Phi} instead of throwing.</p>
 *
 * @since 0.57
 */
public final class DataizedSafe {

    /**
     * Ctor.
     */
    private DataizedSafe() {
        // utility
    }

    /**
     * Take an attribute without throwing; on error return the enclosure.
     * @param receiver Object to take from
     * @param attr Attribute name
     * @return Attribute value or error object
     */
    @SuppressWarnings("PMD.ProhibitPublicStaticMethods")
    public static Phi take(final Phi receiver, final String attr) {
        return PhSafe.takeNav(receiver, attr);
    }

    /**
     * Dataize without throwing; on error return the error object.
     * @param target Object to dataize
     * @return Bytes as phi, or error object
     */
    @SuppressWarnings("PMD.ProhibitPublicStaticMethods")
    public static Phi dataized(final Phi target) {
        return DataizedSafe.whenError(
            () -> new Data.ToPhi(new Dataized(target).take())
        );
    }

    /**
     * Run action and return error enclosure on {@link EOerror.ExError}.
     * @param action Action to run
     * @return Result or error object
     */
    private static Phi whenError(final Supplier<Phi> action) {
        Phi result;
        try {
            result = action.get();
        } catch (final EOerror.ExError ex) {
            result = ex.enclosure();
        }
        return result;
    }
}
