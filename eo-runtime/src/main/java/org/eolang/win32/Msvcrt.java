/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.win32;

import com.sun.jna.Library;
import com.sun.jna.Native;
import com.sun.jna.Pointer;
import com.sun.jna.Structure;
import com.sun.jna.WString;

/**
 * The Microsoft C runtime (msvcrt), exposing the POSIX-compatible file functions.
 *
 * <p>The methods are named after the real msvcrt exports, so most carry a leading
 * underscore ({@code _wopen}, {@code _read}, {@code _wstat64}); the ISO C
 * {@code getenv} and {@code strerror} are the ones without. They return small
 * integer file descriptors, exactly like libc on posix, which is why the win32
 * file objects can thread them through EO the same way the posix ones do. Every
 * function taking a path binds the wide twin and takes a {@link WString}, so a
 * name leaving the active code page still reaches the CRT as it was written,
 * while EO goes on asking for the narrow name it shares with posix.</p>
 *
 * @since 0.74.0
 */
@SuppressWarnings("PMD.MethodNamingConventions")
public interface Msvcrt extends Library {

    /**
     * Singleton.
     */
    Msvcrt INSTANCE = Native.load("msvcrt", Msvcrt.class);

    /**
     * Standard input file descriptor.
     */
    int STDIN_FILENO = 0;

    /**
     * Standard output file descriptor.
     */
    int STDOUT_FILENO = 1;

    /**
     * Open flag for reading only.
     */
    int O_RDONLY = 0x0000;

    /**
     * Open flag for reading and writing.
     */
    int O_RDWR = 0x0002;

    /**
     * Open flag that keeps the bytes untouched, disabling the CRT's text-mode
     * translation between {@code \n} and {@code \r\n}.
     */
    int O_BINARY = 0x8000;

    /**
     * Opens a file and returns a file descriptor.
     *
     * <p>The native {@code _wopen} is variadic, so {@code mode} is a trailing
     * varargs, making JNA use the variadic calling convention.</p>
     *
     * @param path Path to the file
     * @param flags Open flags, e.g. {@code _O_RDONLY | _O_BINARY}
     * @param mode Permission bits used when the flags request file creation
     * @return File descriptor, or -1 on error
     */
    int _wopen(WString path, int flags, Object... mode);

    /**
     * Reads bytes from a file descriptor into the buffer.
     *
     * @param descriptor File descriptor
     * @param buffer Buffer to read into
     * @param size Number of bytes to read
     * @return Number of bytes read, or -1 on error
     */
    int _read(int descriptor, byte[] buffer, int size);

    /**
     * Writes bytes from the buffer to a file descriptor.
     *
     * @param descriptor File descriptor
     * @param buffer Buffer to write from
     * @param size Number of bytes to write
     * @return Number of bytes written, or -1 on error
     */
    int _write(int descriptor, byte[] buffer, int size);

    /**
     * Closes a file descriptor.
     *
     * @param descriptor File descriptor
     * @return Zero on success, -1 on error
     */
    int _close(int descriptor);

    /**
     * Checks a file's accessibility.
     *
     * @param path Path to the file
     * @param mode Accessibility check to perform (0 tests for existence)
     * @return Zero when the check succeeds, -1 on error
     */
    int _waccess(WString path, int mode);

    /**
     * Gets a file's status by path, filling a {@code struct _stat64} whose
     * 64-bit {@code st_size} reports sizes past two gigabytes.
     *
     * @param path Path to the file
     * @param statbuf Structure to fill with the file's metadata
     * @return Zero on success, -1 on error
     */
    int _wstat64(WString path, Structure statbuf);

    /**
     * Deletes a file.
     *
     * @param path Path to the file
     * @return Zero on success, -1 on error
     */
    int _wunlink(WString path);

    /**
     * Removes an empty directory.
     *
     * @param path Path to the directory
     * @return Zero on success, -1 on error
     */
    int _wrmdir(WString path);

    /**
     * Creates a directory, taking no permission bits, so the mode passed at the
     * EO level is ignored here.
     *
     * @param path Path to the directory
     * @return Zero on success, -1 on error
     */
    int _wmkdir(WString path);

    /**
     * Creates a new file, or truncates an existing one, and opens it.
     *
     * @param path Path to the file
     * @param mode Permission bits for a newly created file
     * @return File descriptor on success, -1 on error
     */
    int _wcreat(WString path, int mode);

    /**
     * Renames a file, the wide twin of the ISO C {@code rename} export.
     *
     * @param from Current path of the file
     * @param target New path of the file
     * @return Zero on success, -1 on error
     */
    int _wrename(WString from, WString target);

    /**
     * Returns the process identifier of the calling process, the CRT
     * counterpart of Kernel32's {@code GetCurrentProcessId}.
     *
     * @return Process identifier
     */
    int _getpid();

    /**
     * Gets an environment variable's value, the ISO C export without a leading
     * underscore.
     *
     * @param name Name of the variable
     * @return Value of the variable, or {@code null} when it is not defined
     */
    String getenv(String name);

    /**
     * Gets the current time as seconds and milliseconds since the Unix epoch,
     * filling a {@code struct __timeb64}. It replaces Kernel32's wall-clock
     * {@code GetSystemTime}, lining the win32 clock up with the posix
     * {@code gettimeofday}: like {@code gettimeofday} it hands back the raw
     * status code, unlike the older {@code _ftime} that returns {@code void}.
     * The 64-bit variant is used because the 32-bit {@code _ftime32_s} can
     * only represent dates through 18 January 2038.
     *
     * @param timeb Structure to fill with the current time
     * @return Zero on success, an errno value on failure
     */
    int _ftime64_s(Structure timeb);

    /**
     * Duplicates a file descriptor.
     *
     * @param descriptor File descriptor to duplicate
     * @return New file descriptor, or -1 on error
     */
    int _dup(int descriptor);

    /**
     * Reassigns a file descriptor to refer to the same file as another one.
     *
     * @param descriptor File descriptor to duplicate
     * @param other File descriptor to reassign
     * @return Zero on success, -1 on error
     */
    int _dup2(int descriptor, int other);

    /**
     * Returns a pointer to the calling thread's {@code errno}.
     *
     * <p>The CRT keeps {@code errno} per thread and exposes it through this
     * function; the {@code errno} macro itself expands to {@code (*_errno())}.
     * Reading the pointed-to {@code int} right after a failed CRT call gives the
     * code the call stored, the win32 counterpart of the posix {@code errno}.</p>
     *
     * @return Pointer to the calling thread's {@code errno}
     */
    Pointer _errno();

    /**
     * Converts a CRT {@code errno} value into a human-readable message.
     *
     * @param errnum The error number
     * @return Error as a string
     */
    String strerror(int errnum);
}
