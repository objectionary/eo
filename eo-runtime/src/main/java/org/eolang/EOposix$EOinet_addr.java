/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import com.sun.jna.Native;
import org.eolang.posix.CStdLib;
import org.eolang.sys.Cstring;
import org.eolang.sys.Inaddr;

/**
 * Turns an IPv4 address in text into a number, as POSIX `inet_addr` does.
 *
 * <p>The function leaves `errno` alone on text it cannot convert, so the
 * refusal is reported here instead, as `EINVAL`, or a later `strerror(errno)`
 * reads whatever an unrelated earlier call left behind.</p>
 *
 * @since 0.77.0
 * @checkstyle IllegalIdentifierNameCheck (6 lines)
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "posix.inet-addr")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOposix$EOinet_addr extends PhDefault implements Atom {

    /**
     * Ctor.
     */
    public EOposix$EOinet_addr() {
        super(new Attrs(new Attr("address", new AtVoid("address"))));
    }

    @Override
    public Phi lambda() {
        final String address = new Cstring(Expect.at(this, "address")).it();
        final Inaddr converted = new Inaddr(address, CStdLib.INSTANCE.inet_addr(address));
        if (converted.failed()) {
            Native.setLastError(22);
        }
        return new Data.ToPhi(converted.it());
    }
}
