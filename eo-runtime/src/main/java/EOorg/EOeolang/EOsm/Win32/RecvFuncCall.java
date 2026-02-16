/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

/*
 * @checkstyle PackageNameCheck (4 lines)
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package EOorg.EOeolang.EOsm.Win32; // NOPMD

import EOorg.EOeolang.EOsm.Syscall;
import java.util.Arrays;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.Phi;

/**
 * ReadFile kernel32 function call.
 * @see <a href="https://learn.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-readfile">here for details</a>
 * @since 0.40.0
 */
public final class RecvFuncCall implements Syscall {
    /**
     * Win32 object.
     */
    private final Phi win;

    /**
     * Ctor.
     * @param win Win32 object
     */
    public RecvFuncCall(final Phi win) {
        this.win = win;
    }

    @Override
    public Phi make(final Phi... params) {
        final Phi result = this.win.take("return").copy();
        final int size = new Dataized(params[1]).asNumber().intValue();
        final byte[] buf = new byte[size];
        final int received = Winsock.INSTANCE.recv(
            new Dataized(params[0]).asNumber().intValue(),
            buf,
            size,
            new Dataized(params[2]).asNumber().intValue()
        );
        result.put(0, new Data.ToPhi(received));
        result.put(1, new Data.ToPhi(Arrays.copyOf(buf, received)));
        return result;
    }
}
