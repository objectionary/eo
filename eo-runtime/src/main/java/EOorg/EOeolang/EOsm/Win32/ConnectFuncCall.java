/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

/*
 * @checkstyle PackageNameCheck (4 lines)
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package EOorg.EOeolang.EOsm.Win32; // NOPMD

import EOorg.EOeolang.EOsm.SockaddrIn;
import EOorg.EOeolang.EOsm.Syscall;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.PhDefault;
import org.eolang.Phi;

/**
 * The 'connect' WS2_32 function call.
 * @see <a href="https://learn.microsoft.com/en-us/windows/win32/api/winsock2/nf-winsock2-connect">here for details</a>
 * @since 0.40.0
 */
public final class ConnectFuncCall implements Syscall {
    /**
     * Win32 object.
     */
    private final Phi win;

    /**
     * Ctor.
     * @param win Win32 object
     */
    public ConnectFuncCall(final Phi win) {
        this.win = win;
    }

    @Override
    public Phi make(final Phi... params) {
        final Phi result = this.win.take("return").copy();
        result.put(
            0,
            new Data.ToPhi(
                Winsock.INSTANCE.connect(
                    new Dataized(params[0]).asNumber().intValue(),
                    new SockaddrIn(
                        new Dataized(params[1].take("sin-family")).take(Short.class),
                        new Dataized(params[1].take("sin-port")).take(Short.class),
                        new Dataized(params[1].take("sin-addr")).take(Integer.class),
                        new Dataized(params[1].take("sin-zero")).take()
                    ),
                    new Dataized(params[2]).asNumber().intValue()
                )
            )
        );
        result.put(1, new PhDefault());
        return result;
    }
}
