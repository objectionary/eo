/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
/*
 * @checkstyle PackageNameCheck (4 lines)
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package org.eolang.EOsm.Win32; // NOPMD

import com.sun.jna.FunctionMapper;
import com.sun.jna.Library;
import com.sun.jna.Native;
import java.util.Collections;

/**
 * The Microsoft C runtime (msvcrt), exposing the POSIX-compatible file functions.
 *
 * <p>These functions are exported by the runtime with a leading underscore
 * ({@code _open}, {@code _read}, {@code _close}), so a function mapper prepends
 * it to each declared method name. They return small integer file descriptors,
 * exactly like libc on posix, which is why the win32 file objects can thread
 * them through EO the same way the posix ones do.</p>
 *
 * @since 0.74.0
 */
public interface Msvcrt extends Library {

    /**
     * Singleton.
     */
    Msvcrt INSTANCE = Native.load(
        "msvcrt",
        Msvcrt.class,
        Collections.singletonMap(
            Library.OPTION_FUNCTION_MAPPER,
            (FunctionMapper) (library, method) -> "_".concat(method.getName())
        )
    );

    /**
     * Opens a file and returns a file descriptor.
     * @param path Path to the file
     * @param flags Open flags, e.g. {@code _O_RDONLY | _O_BINARY}
     * @return File descriptor, or -1 on error
     */
    int open(String path, int flags);

    /**
     * Reads bytes from a file descriptor into the buffer.
     * @param descriptor File descriptor
     * @param buffer Buffer to read into
     * @param size Number of bytes to read
     * @return Number of bytes read, or -1 on error
     */
    int read(int descriptor, byte[] buffer, int size);

    /**
     * Writes bytes from the buffer to a file descriptor.
     * @param descriptor File descriptor
     * @param buffer Buffer to write from
     * @param size Number of bytes to write
     * @return Number of bytes written, or -1 on error
     */
    int write(int descriptor, byte[] buffer, int size);

    /**
     * Closes a file descriptor.
     * @param descriptor File descriptor
     * @return Zero on success, -1 on error
     */
    int close(int descriptor);
}
