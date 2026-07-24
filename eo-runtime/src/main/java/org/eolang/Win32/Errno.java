/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
/*
 * @checkstyle PackageNameCheck (4 lines)
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package org.eolang.Win32; // NOPMD

import java.util.function.Supplier;
import org.eolang.Data;
import org.eolang.PhDefault;
import org.eolang.Phi;

/**
 * The {@code output} of a file function call's {@code return}, carrying the OS
 * error reason when the native call failed.
 *
 * <p>The msvcrt file functions report a failure by returning {@code -1} and
 * leaving the real cause in the CRT's per-thread {@code errno}. This object
 * reads it straight away through {@link Msvcrt#_errno()} — before any other CRT
 * call can overwrite it — and turns it into a human-readable message with
 * {@code strerror}. On success it stays an empty {@link PhDefault}, exactly what
 * the wrappers put there before.</p>
 *
 * @since 0.74.0
 */
final class Errno implements Supplier<Phi> {

    /**
     * The code the native call returned.
     */
    private final int code;

    /**
     * Ctor.
     * @param status The code the native call returned
     */
    Errno(final int status) {
        this.code = status;
    }

    @Override
    public Phi get() {
        final Phi output;
        if (this.code == -1) {
            output = new Data.ToPhi(
                Msvcrt.INSTANCE.strerror(Msvcrt.INSTANCE._errno().getInt(0))
            );
        } else {
            output = new PhDefault();
        }
        return output;
    }
}
