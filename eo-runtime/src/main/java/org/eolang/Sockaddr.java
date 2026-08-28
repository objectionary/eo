/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import com.sun.jna.Platform;
import com.sun.jna.Structure;
import java.util.Arrays;

/**
 * A {@code sockaddr_in} laid out the way the running platform lays it out.
 *
 * <p>Linux and Windows open it with a two-byte family, macOS and the BSDs
 * with a one-byte length and a one-byte family, and a caller of {@code bind},
 * {@code connect} or {@code accept} wants whichever the kernel reads.</p>
 *
 * @since 0.74.0
 * @todo #7748:35min Take the reading of an address out of the syscalls.
 *  `BindSyscall`, `ConnectSyscall` and `AcceptSyscall` each dataize the same
 *  four attributes of the same EO `sockaddr` object to build this one. That
 *  belongs in one place, most likely a ctor here that takes the `Phi`.
 */
public final class Sockaddr {

    /**
     * Address family (e.g., AF_INET).
     */
    private final short family;

    /**
     * Port number in network byte order.
     */
    private final short port;

    /**
     * IP address in network byte order.
     */
    private final int addr;

    /**
     * Padding to match C structure.
     */
    private final byte[] zero;

    /**
     * Ctor.
     * @param family Family
     * @param port Port
     * @param addr Address
     * @param zero Zero 8 bytes
     */
    public Sockaddr(final short family, final short port, final int addr, final byte[] zero) {
        this.family = family;
        this.port = port;
        this.addr = addr;
        this.zero = Arrays.copyOf(zero, zero.length);
    }

    /**
     * The struct itself.
     * @return The structure, in the layout of the platform
     */
    public Structure it() {
        final Structure found;
        if (Platform.isMac()) {
            found = new MacSockaddrIn(this.family, this.port, this.addr, this.zero);
        } else {
            found = new SockaddrIn(this.family, this.port, this.addr, this.zero);
        }
        return found;
    }
}
