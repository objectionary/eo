/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
/*
 * @checkstyle PackageNameCheck (4 lines)
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package EOorg.EOeolang.EOsm; // NOPMD

import com.sun.jna.Structure;
import java.util.Arrays;
import java.util.List;

/**
 * The sockaddr_in structure.
 * @since 0.40.0
 * @checkstyle VisibilityModifierCheck (50 lines)
 * @checkstyle ParameterNumberCheck (50 lines)
 */
@SuppressWarnings({"PMD.OnlyOneConstructorShouldDoInitialization", "PMD.DataClass"})
public final class SockaddrIn extends Structure {
    /**
     * Address family (e.g., AF_INET).
     */
    public short family;

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
     */
    public SockaddrIn() {
        super();
        this.zero = new byte[8];
    }

    /**
     * Convenient ctor for testing.
     * @param family Family
     * @param port Port
     * @param addr Address
     */
    public SockaddrIn(final short family, final short port, final int addr) {
        this(family, port, addr, new byte[] {0, 0, 0, 0, 0, 0, 0, 0});
    }

    /**
     * Ctor.
     * @param family Family
     * @param port Port
     * @param addr Address
     * @param zero Zero 8 bytes
     */
    public SockaddrIn(final short family, final short port, final int addr, final byte[] zero) {
        super();
        this.family = family;
        this.port = port;
        this.addr = addr;
        this.zero = Arrays.copyOf(zero, zero.length);
    }

    @Override
    public List<String> getFieldOrder() {
        return Arrays.asList("family", "port", "addr", "zero");
    }
}
