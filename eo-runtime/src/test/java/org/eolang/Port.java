/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import java.io.IOException;
import java.net.InetAddress;
import java.net.ServerSocket;

/**
 * TCP port the operating system reports as free.
 * @since 0.74.0
 */
public final class Port {

    /**
     * Address the port must be free at.
     */
    private final InetAddress address;

    /**
     * Ctor, takes the loopback interface.
     */
    public Port() {
        this(InetAddress.getLoopbackAddress());
    }

    /**
     * Ctor.
     * @param address Address the port must be free at
     */
    public Port(final InetAddress address) {
        this.address = address;
    }

    /**
     * Port number.
     * @return Port number
     */
    public int number() {
        try (ServerSocket socket = new ServerSocket(0, 1, this.address)) {
            return socket.getLocalPort();
        } catch (final IOException exception) {
            throw new IllegalStateException(
                String.format("The system has no free port at %s", this.address),
                exception
            );
        }
    }
}
