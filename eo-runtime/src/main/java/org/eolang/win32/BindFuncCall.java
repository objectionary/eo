/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.win32;

import com.sun.jna.Pointer;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.Handle;
import org.eolang.Int;
import org.eolang.PhDefault;
import org.eolang.Phi;
import org.eolang.SockaddrIn;
import org.eolang.Syscall;

/**
 * The socket WS2_32 function call.
 * @see <a href="https://learn.microsoft.com/en-us/windows/win32/api/winsock2/nf-winsock2-socket">here for details</a>
 * @since 0.40.0
 */
public final class BindFuncCall implements Syscall {

    /**
     * Win32 object.
     */
    private final Phi win;

    /**
     * Ctor.
     * @param win Win32 object
     */
    public BindFuncCall(final Phi win) {
        this.win = win;
    }

    @Override
    public Phi make(final Phi... params) {
        final int length = new Int(
            "the 'length' argument of bind", params[2]
        ).it();
        final Phi result = this.win.take("return").copy();
        result.put(
            0,
            new Data.ToPhi(
                Winsock.INSTANCE.bind(
                    new Pointer(new Handle("the socket of bind", params[0]).it()),
                    new SockaddrIn(
                        new Dataized(params[1].take("family")).take(Short.class),
                        new Dataized(params[1].take("port")).take(Short.class),
                        new Dataized(params[1].take("address")).take(Integer.class),
                        new Dataized(params[1].take("padding")).take()
                    ),
                    length
                )
            )
        );
        result.put(1, new PhDefault());
        return result;
    }
}
