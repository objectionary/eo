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
 * Server on a free port, shared by socket syscall tests.
 *
 * <p>Asks the kernel for an ephemeral port (bind to port 0) instead of
 * guessing a candidate from a fixed range, now that {@code socket.eo}'s
 * {@code htons} covers the full 0-65535 range (#6133).</p>
 *
 * @since 0.40.0
 */
@SuppressWarnings("PMD.AvoidUsingHardCodedIP")
public final class RandomServer implements AutoCloseable {

    /**
     * Server socket, bound the moment this object is built.
     */
    private final ServerSocket socket;

    /**
     * Ctor: binds to a kernel-assigned free port on localhost.
     * @checkstyle ConstructorsCodeFreeCheck (5 lines)
     */
    public RandomServer() throws IOException {
        this.socket = this.bound();
    }

    @Override
    public void close() throws IOException {
        if (!this.socket.isClosed()) {
            this.socket.close();
        }
    }

    /**
     * The port the server is bound to.
     * @return Port
     */
    public int port() {
        return this.socket.getLocalPort();
    }

    /**
     * Bind a server socket to a kernel-assigned free port.
     * @return Bound server socket
     */
    private ServerSocket bound() throws IOException {
        final ServerSocket opened = new ServerSocket();
        opened.setReuseAddress(true);
        opened.bind(new InetSocketAddress("127.0.0.1", 0));
        Logger.debug(this, "Server bound on port %d", opened.getLocalPort());
        return opened;
    }
}
