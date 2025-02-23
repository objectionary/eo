/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

/*
 * @checkstyle PackageNameCheck (4 lines)
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package EOorg.EOeolang.EOsys.Win32; // NOPMD

import EOorg.EOeolang.EOsys.Syscall;
import org.eolang.Data;
import org.eolang.PhDefault;
import org.eolang.Phi;

/**
 * WSAGetLastError WS2_32 function call.
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
     * @param win Win32 object
     */
    public WSAGetLastErrorFuncCall(final Phi win) {
        this.win = win;
    }

    @Override
    public Phi make(final Phi... params) {
        final Phi result = this.win.take("return").copy();
        result.put(0, new Data.ToPhi(Winsock.INSTANCE.WSAGetLastError()));
        result.put(1, new PhDefault());
        return result;
    }
}
