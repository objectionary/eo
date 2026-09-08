/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.win32;

import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.PhDefault;
import org.eolang.Phi;
import org.eolang.Syscall;

/**
 * WSAStartup WS2_32 function call.
 *
 * @see <a href="https://learn.microsoft.com/en-us/windows/win32/api/winsock2/nf-winsock2-wsastartup">here for details</a>
 * @since 0.40.0
 * @checkstyle AbbreviationAsWordInNameCheck (100 lines)
 */
public final class WSAStartupFuncCall implements Syscall {

    /**
     * Win32 object.
     */
    private final Phi win;

    /**
     * Ctor.
     *
     * @param win Win32 object
     */
    public WSAStartupFuncCall(final Phi win) {
        this.win = win;
    }

    @Override
    public Phi make(final Phi... params) {
        final Phi result = this.win.take("return").copy();
        result.put(
            0,
            new Data.ToPhi(
                Winsock.INSTANCE.WSAStartup(
                    new Dataized(params[0]).take(Short.class),
                    new WSAData()
                )
            )
        );
        result.put(1, new PhDefault());
        return result;
    }
}
