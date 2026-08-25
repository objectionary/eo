/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.win32;

import com.sun.jna.Pointer;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.PhDefault;
import org.eolang.Phi;
import org.eolang.Syscall;

/**
 * The socket WS2_32 function call.
 * @see <a href="https://learn.microsoft.com/en-us/windows/win32/api/winsock2/nf-winsock2-socket">here for details</a>
 * @since 0.40.0
 */
public final class SocketFuncCall implements Syscall {

    /**
     * Win32 object.
     */
    private final Phi win;

    /**
     * Ctor.
     * @param win Win32 object
     */
    public SocketFuncCall(final Phi win) {
        this.win = win;
    }

    @Override
    public Phi make(final Phi... params) {
        final Phi result = this.win.take("return").copy();
        result.put(
            0,
            new Data.ToPhi(
                Pointer.nativeValue(
                    Winsock.INSTANCE.socket(
                        new Dataized(params[0]).asNumber().intValue(),
                        new Dataized(params[1]).asNumber().intValue(),
                        new Dataized(params[2]).asNumber().intValue()
                    )
                )
            )
        );
        result.put(1, new PhDefault());
        return result;
    }
}
