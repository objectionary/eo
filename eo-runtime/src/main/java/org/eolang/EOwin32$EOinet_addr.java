/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import com.sun.jna.Native;
import java.nio.charset.StandardCharsets;
import org.eolang.sys.Cstring;
import org.eolang.sys.Inaddr;
import org.eolang.sys.win32.Winsock;

/**
 * Turns an IPv4 address in text into a number, as Winsock `inet_addr` does.
 *
 * <p>The text is handed to `ws2_32` rather than read here, because
 * `inet_addr` takes four forms — `a.b.c.d`, `a.b.c`, `a.b` and `a` — and
 * reads every part the way C does, octal behind a leading zero and
 * hexadecimal behind a leading `0x`. A parser of our own accepted only the
 * first form and read every part as decimal, so the two halves of this call
 * answered the same program differently: `010.1.1.1` is 8.1.1.1 to both C
 * libraries and was 10.1.1.1 here, a different host with no error on either
 * side, and `127.1` was refused outright (#7512).</p>
 *
 * @see <a href="https://learn.microsoft.com/en-us/windows/win32/api/winsock2/nf-winsock2-inet_addr">here for details</a>
 * @since 0.77.0
 * @checkstyle IllegalIdentifierNameCheck (6 lines)
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "win32.inet-addr")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOwin32$EOinet_addr extends PhDefault implements Atom {

    /**
     * Ctor.
     */
    public EOwin32$EOinet_addr() {
        super(new Attrs(new Attr("address", new AtVoid("address"))));
    }

    @Override
    public Phi lambda() {
        final String address = new Cstring(Expect.at(this, "address")).it();
        final Inaddr converted = new Inaddr(
            address,
            Winsock.INSTANCE.inet_addr(Native.toByteArray(address, StandardCharsets.UTF_8))
        );
        if (converted.failed()) {
            Winsock.INSTANCE.WSASetLastError(Winsock.WSAEINVAL);
        }
        return new Data.ToPhi(converted.it());
    }
}
