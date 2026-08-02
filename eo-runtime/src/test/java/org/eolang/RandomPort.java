/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import com.jcabi.log.Logger;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.net.ServerSocket;

/**
 * A free port number, shared by socket syscall tests.
 *
 * <p>Asks the kernel for an ephemeral port and releases it right away, so
 * the caller gets a real currently-free port instead of a guess from a
 * fixed range.</p>
 *
 * @since 0.40.0
 */
public final class RandomPort {

    /**
     * Pick a free port.
     * @return Free port
     */
    public int pick() {
        try (ServerSocket socket = new ServerSocket(0)) {
            final int port = socket.getLocalPort();
            Logger.debug(this, "Picked free port %d", port);
            return port;
        } catch (final IOException exception) {
            throw new UncheckedIOException(exception);
        }
    }
}
