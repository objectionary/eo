/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.win32;

import com.sun.jna.Library;
import com.sun.jna.Native;
import com.sun.jna.Pointer;
import com.sun.jna.WString;

/**
 * The Windows kernel (kernel32), exposing what the C runtime keeps hidden.
 *
 * <p>The C runtime also has no way to walk a directory that keeps the names as
 * they were written, so the search functions are bound here too, in their wide
 * form.</p>
 *
 * <p>Msvcrt sees a symbolic link as the file behind it, since {@code _stat64}
 * walks the reparse point through and no {@code _lstat} stands beside it. The
 * attribute bits of a path tell the two apart, and only the Windows API hands
 * them over. The wide variant is bound, so a path carrying anything outside the
 * active code page still reaches the kernel as it was written.</p>
 *
 * @since 0.75.0
 */
@SuppressWarnings("PMD.MethodNamingConventions")
public interface Kernel32 extends Library {

    /**
     * Singleton.
     */
    Kernel32 INSTANCE = Native.load("kernel32", Kernel32.class);

    /**
     * Gets the attribute bits of a path, telling of the link itself rather
     * than of the file it points at.
     * @param path Path to the file
     * @return Attribute bits, or -1 on error
     */
    int GetFileAttributesW(WString path);

    /**
     * Starts a search over the entries a pattern matches and reports the
     * first of them.
     * @param pattern The pattern, a directory with a trailing wildcard
     * @param data Filled in with the entry that was found
     * @return Handle of the search, or INVALID_HANDLE_VALUE on error
     */
    Pointer FindFirstFileW(WString pattern, WinFindData data);

    /**
     * Reports the next entry of a search.
     * @param search Handle of the search
     * @param data Filled in with the entry that was found
     * @return False when there is nothing left to find
     */
    boolean FindNextFileW(Pointer search, WinFindData data);

    /**
     * Closes a search.
     * @param search Handle of the search
     * @return False on error
     */
    boolean FindClose(Pointer search);
}
