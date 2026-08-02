/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

/**
 * Port number in network byte order, shared by socket syscall tests.
 * @since 0.40.0
 */
public final class NetworkPort {

    /**
     * Port number in host byte order.
     */
    private final int port;

    /**
     * Ctor.
     * @param port Port number in host byte order
     */
    public NetworkPort(final int port) {
        this.port = port;
    }

    /**
     * Convert to network byte order (htons).
     * @return Port number in network byte order
     */
    public short bytes() {
        return (short) (((this.port & 0xFF) << 8) | ((this.port >> 8) & 0xFF));
    }
}
