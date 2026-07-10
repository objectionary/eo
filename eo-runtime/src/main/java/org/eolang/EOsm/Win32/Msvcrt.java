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
import com.sun.jna.Structure;
import java.util.Collections;

/**
 * The Microsoft C runtime (msvcrt), exposing the POSIX-compatible file functions.
 *
 * <p>Most of these functions are exported by the runtime with a leading
 * underscore ({@code _open}, {@code _read}, {@code _close}), so a function
 * mapper prepends it to each declared method name. The exception is the ISO C
 * {@code rename}, which the runtime exports without an underscore, so the
 * mapper leaves it alone. They return small integer file descriptors, exactly
 * like libc on posix, which is why the win32 file objects can thread them
 * through EO the same way the posix ones do.</p>
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
            (FunctionMapper) (library, method) -> {
                final String name = method.getName();
                final String mapped;
                if ("rename".equals(name)) {
                    mapped = name;
                } else {
                    mapped = "_".concat(name);
                }
                return mapped;
            }
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

    /**
     * Checks a file's accessibility. Maps to {@code _access}.
     * @param path Path to the file
     * @param mode Accessibility check to perform (0 tests for existence)
     * @return Zero when the check succeeds, -1 on error
     */
    int access(String path, int mode);

    /**
     * Gets a file's status by path. Maps to {@code _stat}, whose
     * {@code struct _stat} carries a 32-bit {@code st_size}, so it reports
     * sizes only up to two gigabytes.
     * @param path Path to the file
     * @param statbuf Structure to fill with the file's metadata
     * @return Zero on success, -1 on error
     */
    int stat(String path, Structure statbuf);

    /**
     * Deletes a file. Maps to {@code _unlink}.
     * @param path Path to the file
     * @return Zero on success, -1 on error
     */
    int unlink(String path);

    /**
     * Removes an empty directory. Maps to {@code _rmdir}.
     * @param path Path to the directory
     * @return Zero on success, -1 on error
     */
    int rmdir(String path);

    /**
     * Creates a new file, or truncates an existing one, and opens it. Maps to
     * {@code _creat}.
     * @param path Path to the file
     * @param mode Permission bits for a newly created file
     * @return File descriptor on success, -1 on error
     */
    int creat(String path, int mode);

    /**
     * Renames a file. Maps to the ISO C {@code rename}, without a leading
     * underscore.
     * @param from Current path of the file
     * @param target New path of the file
     * @return Zero on success, -1 on error
     */
    int rename(String from, String target);
}
