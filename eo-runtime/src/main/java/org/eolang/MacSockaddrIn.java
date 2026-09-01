/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import com.sun.jna.Structure;
import java.util.Arrays;
import java.util.List;

/**
 * The sockaddr_in structure, as macOS and the BSDs lay it out: a one-byte
 * {@code sin_len} and a one-byte {@code sin_family}, not a two-byte family.
 * @since 0.74.0
 * @checkstyle VisibilityModifierCheck (50 lines)
 */
public final class MacSockaddrIn extends Structure {

    /**
     * Length of the structure.
     */
    public byte len;

    /**
     * Address family (e.g., AF_INET).
     */
    public byte family;

    /**
     * Port number in network byte order.
     */
    public short port;

    /**
     * IP address in network byte order.
     */
    public int addr;

    /**
     * Padding to match C structure.
     */
    public byte[] zero;

    /**
     * Ctor.
     * @param family Family
     * @param port Port
     * @param addr Address
     * @param zero Zero 8 bytes
     */
    public MacSockaddrIn(
        final short family, final short port, final int addr, final byte[] zero
    ) {
        this(new Padding(zero).bytes(), family, port, addr);
    }

    /**
     * Primary ctor, receiving an already validated padding. The padding
     * comes first so this signature differs from the public one above.
     * @param zero Validated padding
     * @param family Family
     * @param port Port
     * @param addr Address
     */
    private MacSockaddrIn(
        final byte[] zero, final short family, final short port, final int addr
    ) {
        super();
        this.len = 16;
        this.family = (byte) family;
        this.port = port;
        this.addr = addr;
        this.zero = zero;
    }

    @Override
    public List<String> getFieldOrder() {
        return Arrays.asList("len", "family", "port", "addr", "zero");
    }
}
