/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import com.jcabi.log.Logger;
import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.ServerSocket;

/**
 * Server on random port, shared by socket syscall tests.
 * @since 0.40.0
 */
@SuppressWarnings("PMD.AvoidUsingHardCodedIP")
public final class RandomServer {

    /**
     * Server socket.
     */
    private ServerSocket socket;

    /**
     * Port.
     */
    private int port;

    /**
     * Start server on random port.
     * @return Self
     */
    public RandomServer started() {
        boolean bound = false;
        while (!bound) {
            this.port = new RandomPort().pick();
            try {
                this.socket = new ServerSocket();
                this.socket.setReuseAddress(true);
                this.socket.bind(new InetSocketAddress("127.0.0.1", this.port));
                bound = true;
                Logger.debug(this, "Server started on port %d", this.port);
            } catch (final IOException exception) {
                Logger.debug(this, "Port %d is unavailable, trying another port...", this.port);
            }
        }
        return this;
    }

    /**
     * Close server socket.
     */
    public void stop() throws IOException {
        if (this.socket != null && !this.socket.isClosed()) {
            this.socket.close();
        }
    }

    /**
     * The port the server is bound to.
     * @return Port
     */
    public int port() {
        return this.port;
    }
}
