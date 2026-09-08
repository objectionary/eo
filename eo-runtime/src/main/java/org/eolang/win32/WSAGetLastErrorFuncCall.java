/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.win32;

import com.sun.jna.Native;
import org.eolang.Data;
import org.eolang.PhDefault;
import org.eolang.Phi;
import org.eolang.Syscall;

/**
 * WSAGetLastError WS2_32 function call.
 *
 * <p>The code comes from {@link Native#getLastError()} rather than from
 * {@code ws2_32}, because the last error lives in a per-thread slot that the
 * JNI and JNA machinery between the two calls overwrites: by the time a
 * mapped {@code WSAGetLastError} runs, the slot holds whatever that machinery
 * left there, usually zero. JNA keeps a copy of the slot taken the instant
 * every mapped call returns, and that copy is what an EO program has to read.
 * JNA itself does the same for Kernel32's {@code GetLastError}, which it
 * short-circuits to the very same copy.</p>
 *
 * @see <a href="https://learn.microsoft.com/en-us/windows/win32/api/winsock2/nf-winsock2-wsagetlasterror">here for details</a>
 * @since 0.40.0
 * @checkstyle AbbreviationAsWordInNameCheck (5 lines)
 */
public final class WSAGetLastErrorFuncCall implements Syscall {

    /**
     * Win32 object.
     */
    private final Phi win;

    /**
     * Ctor.
     *
     * @param win Win32 object
     */
    public WSAGetLastErrorFuncCall(final Phi win) {
        this.win = win;
    }

    @Override
    public Phi make(final Phi... params) {
        final Phi result = this.win.take("return").copy();
        result.put(0, new Data.ToPhi(Native.getLastError()));
        result.put(1, new PhDefault());
        return result;
    }
}
