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
 * <p>Picks a port from a fixed, low range rather than asking the kernel
 * for an ephemeral one, because {@code socket.eo}'s {@code htons} only
 * supports ports up to 32767 until #6133 lands; the kernel's own
 * ephemeral range starts above that limit.</p>
 *
 * @since 0.40.0
 */
@SuppressWarnings("PMD.AvoidUsingHardCodedIP")
public final class RandomServer {

    /**
     * Maximum bind attempts before giving up.
     */
    private static final int MAX_ATTEMPTS = 50;

    /**
     * Server socket, bound the moment this object is built.
     */
    private final ServerSocket socket;

    /**
     * Ctor: binds to a free port on localhost, trying up to
     * {@link RandomServer#MAX_ATTEMPTS} candidates before giving up.
     * @checkstyle ConstructorsCodeFreeCheck (5 lines)
     */
    public RandomServer() throws IOException {
        this.socket = this.bound();
    }

    /**
     * Close server socket.
     */
    public void stop() throws IOException {
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
     * Bind a server socket to a free port, retrying a bounded number of
     * times so a persistent bind failure fails the test instead of
     * hanging the build forever.
     * @return Bound server socket
     */
    private ServerSocket bound() throws IOException {
        IOException last = null;
        for (int attempt = 0; attempt < RandomServer.MAX_ATTEMPTS; attempt = attempt + 1) {
            final int candidate = new RandomPort().pick();
            try {
                final ServerSocket opened = new ServerSocket();
                opened.setReuseAddress(true);
                opened.bind(new InetSocketAddress("127.0.0.1", candidate));
                Logger.debug(this, "Server bound on port %d", candidate);
                return opened;
            } catch (final IOException exception) {
                Logger.debug(this, "Port %d is unavailable, trying another port...", candidate);
                last = exception;
            }
        }
        throw new IOException(
            String.format(
                "Could not bind to a free port after %d attempts",
                RandomServer.MAX_ATTEMPTS
            ),
            last
        );
    }
}
