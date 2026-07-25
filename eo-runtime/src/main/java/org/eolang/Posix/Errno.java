/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
/*
 * @checkstyle PackageNameCheck (4 lines)
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package org.eolang.Posix; // NOPMD

import com.sun.jna.Native;
import java.util.function.Supplier;
import org.eolang.Data;
import org.eolang.PhDefault;
import org.eolang.Phi;

/**
 * The {@code output} of a file syscall's {@code return}, carrying the OS error
 * reason when the native call failed.
 *
 * <p>libc reports a failure by returning {@code -1} and leaving the real cause
 * in {@code errno}. This object reads {@code errno} straight away through
 * {@link Native#getLastError()}, which JNA captures the instant the native call
 * returns, and turns it into a human-readable message with {@code strerror}.
 * That is why the wrapper builds it immediately after the call, before any other
 * native call can overwrite the value. On success it stays an empty
 * {@link PhDefault}, exactly what the wrappers put there before.</p>
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
                CStdLib.INSTANCE.strerror(Native.getLastError())
            );
        } else {
            output = new PhDefault();
        }
        return output;
    }
}
