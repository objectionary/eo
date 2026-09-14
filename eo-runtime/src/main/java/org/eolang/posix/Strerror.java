/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.posix;

import com.sun.jna.Native;
import java.util.function.IntFunction;

/**
 * The human-readable message of an OS error code, taken without losing the
 * error the failing call has left behind.
 *
 * <p>Looking a message up is a native call of its own, and JNA remembers the
 * {@code errno} of the call it made last: {@code strerror} therefore overwrites
 * the very number the program is about to read through {@link ErrnoSyscall}.
 * It really does — the first lookup in the process makes libc probe the message
 * catalogs of the current locale, they are not there, and {@code ENOENT} lands
 * where {@code EEXIST} was. Every later lookup finds the catalog answer cached
 * and leaves {@code errno} alone, so the loss hits whichever failing call in
 * the process happens to be the first one, and nobody else. This object reads
 * the number before the lookup and puts it back after, making the translation
 * invisible to the program.</p>
 *
 * @since 0.75
 */
final class Strerror {

    /**
     * Where a message comes from, by error code.
     */
    private final IntFunction<String> messages;

    /**
     * The code to translate.
     */
    private final int errno;

    /**
     * Ctor.
     *
     * @param code The code to translate
     */
    Strerror(final int code) {
        this(CStdLib.INSTANCE::strerror, code);
    }

    /**
     * Ctor.
     *
     * @param source Where a message comes from, by error code
     * @param code The code to translate
     */
    Strerror(final IntFunction<String> source, final int code) {
        this.messages = source;
        this.errno = code;
    }

    /**
     * The message.
     *
     * @return The error as a human-readable string
     */
    String it() {
        final int last = Native.getLastError();
        final String result = this.messages.apply(this.errno);
        Native.setLastError(last);
        return result;
    }
}
