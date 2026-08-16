/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import codes.ivanov.ephpo.Ephemeral;
import codes.ivanov.ephpo.EphemeralResolver;
import java.io.IOException;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.ServerSocket;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link EOguarded}, through the {@code socket} object that
 * relies on it to release its descriptors.
 * @since 0.74.0
 */
@SuppressWarnings("PMD.AvoidUsingHardCodedIP")
@ExtendWith(EphemeralResolver.class)
final class EOguardedTest {

    @Test
    void releasesTheListeningSocketWhenScopeFails(@Ephemeral final int port) {
        final Phi socket = Phi.Φ.take("socket").copy();
        socket.put(0, new Data.ToPhi("127.0.0.1"));
        socket.put(1, new Data.ToPhi(port));
        final Phi listen = socket.take("listen").copy();
        listen.put(0, new EOguardedTest.Failing());
        Assertions.assertThrows(
            ExAbstract.class,
            () -> new Dataized(listen).take(),
            "a failing listen scope must still propagate, but it didn't"
        );
        Assertions.assertDoesNotThrow(
            () -> this.bind(port),
            "the port had to be free again after the failing scope, but it wasn't"
        );
    }

    /**
     * Binds a server to the port and releases it right away.
     * @param port Port to bind to
     * @throws IOException If fails to bind
     */
    private void bind(final int port) throws IOException {
        try (ServerSocket server = new ServerSocket()) {
            server.setReuseAddress(true);
            server.bind(new InetSocketAddress(InetAddress.getLoopbackAddress(), port));
        }
    }

    /**
     * Scoped object that always fails.
     * T "scope failed on purpose" > [s]
     * @since 0.74.0
     */
    private static final class Failing extends PhDefault implements Atom {

        /**
         * Ctor.
         */
        Failing() {
            super(new Attrs(new Attr("s", new AtVoid("s"))));
        }

        @Override
        public Phi lambda() {
            throw new ExFailure("scope failed on purpose");
        }
    }
}
