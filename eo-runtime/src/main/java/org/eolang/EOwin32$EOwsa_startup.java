/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import org.eolang.sys.win32.WSAData;
import org.eolang.sys.win32.Winsock;

/**
 * Starts the sockets subsystem up, as Winsock `WSAStartup` does.
 *
 * <p>The block the function fills in with what it settled on is made here and
 * dropped, since nothing in EO asks about it: what a caller needs to know is
 * whether the version it named was agreed to, and that is the answer.</p>
 *
 * @since 0.77.0
 * @checkstyle IllegalIdentifierNameCheck (6 lines)
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "win32.wsa-startup")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOwin32$EOwsa_startup extends PhDefault implements Atom {

    /**
     * Ctor.
     */
    public EOwin32$EOwsa_startup() {
        super(new Attrs(new Attr("version", new AtVoid("version"))));
    }

    @Override
    public Phi lambda() {
        return new Data.ToPhi(
            Winsock.INSTANCE.WSAStartup(
                new Dataized(this.take("version")).take(Short.class),
                new WSAData()
            )
        );
    }
}
