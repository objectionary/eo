/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.win32;

import com.sun.jna.Native;
import java.nio.charset.StandardCharsets;
import java.util.Collections;
import org.eolang.Cstring;
import org.eolang.Data;
import org.eolang.PhDefault;
import org.eolang.Phi;
import org.eolang.Syscall;

/**
 * The 'inet_addr' WS2_32 function call.
 *
 * <p>The text is handed to {@code ws2_32} rather than read here, because
 * {@code inet_addr} takes four forms — {@code a.b.c.d}, {@code a.b.c},
 * {@code a.b} and {@code a} — and reads every part the way C does, octal behind
 * a leading zero and hexadecimal behind a leading {@code 0x}. A parser of our
 * own accepted only the first form and read every part as decimal, so the two
 * halves of this call answered the same program differently: {@code 010.1.1.1}
 * is 8.1.1.1 to both C libraries and was 10.1.1.1 here, a different host with
 * no error on either side, and {@code 127.1} was refused outright (#7512).</p>
 *
 * @see <a href="https://learn.microsoft.com/en-us/windows/win32/api/winsock2/nf-winsock2-inet_addr">here for details</a>
 * @since 0.40.0
 */
public final class InetAddrFuncCall implements Syscall {

    /**
     * The limited-broadcast address, whose conversion is {@code INADDR_NONE}
     * — the same value {@code inet_addr} answers with for text it cannot
     * convert, which is why the two are told apart by the text rather than by
     * the result. The POSIX half of this call makes the same comparison for
     * the same reason.
     */
    private static final String BROADCAST = String.join(
        ".", Collections.nCopies(4, "255")
    );

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
        final String address = new Cstring(
            "the 'address' argument of inet_addr", params[0]
        ).it();
        final int converted = Winsock.INSTANCE.inet_addr(
            Native.toByteArray(address, StandardCharsets.UTF_8)
        );
        if (converted == -1 && !InetAddrFuncCall.BROADCAST.equals(address)) {
            Winsock.INSTANCE.WSASetLastError(Winsock.WSAEINVAL);
        }
        result.put(0, new Data.ToPhi(Integer.toUnsignedLong(converted)));
        result.put(1, new PhDefault());
        return result;
    }
}
