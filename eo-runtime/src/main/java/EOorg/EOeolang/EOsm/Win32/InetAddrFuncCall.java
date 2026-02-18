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
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.nio.ByteBuffer;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.PhDefault;
import org.eolang.Phi;

/**
 * The 'inet_addr' WS2_32 function call.
 * @see <a href="https://learn.microsoft.com/en-us/windows/win32/api/winsock2/nf-winsock2-inet_addr">here for details</a>
 * @since 0.40.0
 * @checkstyle AbbreviationAsWordInNameCheck (100 lines)
 */
public final class InetAddrFuncCall implements Syscall {
    /**
     * Win32 object.
     */
    private final Phi win;

    /**
     * Ctor.
     * @param win Win32 object
     */
    public InetAddrFuncCall(final Phi win) {
        this.win = win;
    }

    @Override
    public Phi make(final Phi... params) {
        final Phi result = this.win.take("return").copy();
        try {
            final ByteBuffer buffer = ByteBuffer.allocate(4);
            buffer.put(
                InetAddress.getByName(new Dataized(params[0]).asString()).getAddress()
            );
            result.put(0, new Data.ToPhi(Integer.reverseBytes(buffer.getInt(0))));
        } catch (final UnknownHostException exception) {
            Kernel32.INSTANCE.SetLastError(Winsock.WSAEINVAL);
            result.put(0, new Data.ToPhi(-1));
        }
        result.put(1, new PhDefault());
        return result;
    }
}
