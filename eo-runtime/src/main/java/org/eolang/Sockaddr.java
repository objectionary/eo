/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import com.sun.jna.Platform;
import com.sun.jna.Structure;

/**
 * A {@code sockaddr_in} laid out the way the running platform lays it out.
 *
 * <p>Linux and Windows open it with a two-byte family, macOS and the BSDs
 * with a one-byte length and a one-byte family, and a caller of {@code bind},
 * {@code connect} or {@code accept} wants whichever the kernel reads.</p>
 *
 * @since 0.74.0
 */
public final class Sockaddr {

    /**
     * The EO object holding the address.
     */
    private final Phi origin;

    /**
     * Ctor.
     * @param phi The EO object holding the address
     */
    public Sockaddr(final Phi phi) {
        this.origin = phi;
    }

    /**
     * The struct itself.
     * @return The structure, in the layout of the platform
     */
    public Structure it() {
        final short family = new Dataized(this.origin.take("family")).take(Short.class);
        final short port = new Dataized(this.origin.take("port")).take(Short.class);
        final int addr = new Dataized(this.origin.take("address")).take(Integer.class);
        final byte[] zero = new Dataized(this.origin.take("padding")).take();
        final Structure found;
        if (Platform.isMac()) {
            found = new MacSockaddrIn(family, port, addr, zero);
        } else {
            found = new SockaddrIn(family, port, addr, zero);
        }
        return found;
    }
}
