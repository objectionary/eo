/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.concurrent.atomic.AtomicReference;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for the {@code socket} object.
 * @since 0.40
 */
@SuppressWarnings("PMD.AvoidUsingHardCodedIP")
final class SyscallTest {

    @Test
    void connectsToLocalServerViaSocketObject() throws IOException {
        final RandomServer server = new RandomServer();
        try {
            final Phi socket = Phi.Φ.take("socket").copy();
            socket.put(0, new Data.ToPhi(this.localhost()));
            socket.put(1, new Data.ToPhi(server.port()));
            final Phi connected = socket.take("connect").copy();
            connected.put(0, new SyscallTest.Simple());
            final byte[] actual = new Dataized(connected).take();
            MatcherAssert.assertThat(
                String.format(
                    "The 'socket.connect' should have been successfully connected to local server, but it didn't, reason: %s",
                    new String(actual, StandardCharsets.UTF_8)
                ),
                actual,
                Matchers.equalTo(new byte[]{1})
            );
        } finally {
            server.stop();
        }
    }

    @Test
    void returnsFallbackWhenConnectionIsRefused() throws IOException {
        final RandomServer refused = new RandomServer();
        refused.stop();
        final Phi socket = Phi.Φ.take("socket").copy();
        socket.put(0, new Data.ToPhi(this.localhost()));
        socket.put(1, new Data.ToPhi(refused.port()));
        final Phi connect = socket.take("connect").copy();
        connect.put(0, new SyscallTest.Simple());
        connect.put(1, new SyscallTest.Simple());
        MatcherAssert.assertThat(
            "connecting to a refused port should have yielded the cant-connect fallback instead of terminating, but it didnt",
            new Dataized(connect).take(),
            Matchers.equalTo(new byte[]{1})
        );
    }

    @Test
    void tellsTheFallbackWhichAddressItFailedToReach() throws IOException {
        final RandomServer refused = new RandomServer();
        refused.stop();
        final Phi socket = Phi.Φ.take("socket").copy();
        socket.put(0, new Data.ToPhi(this.localhost()));
        socket.put(1, new Data.ToPhi(refused.port()));
        final Phi connect = socket.take("connect").copy();
        connect.put(1, Phi.Φ.take("dataized").copy());
        MatcherAssert.assertThat(
            "the refused connection should have told the fallback which address it failed to reach, but it didnt",
            new Dataized(connect).asString(),
            Matchers.containsString(String.format("%s:%d", this.localhost(), refused.port()))
        );
    }

    @Test
    void tellsTheFallbackWhichAddressItFailedToBind() throws IOException {
        final RandomServer taken = new RandomServer();
        try {
            final Phi socket = Phi.Φ.take("socket").copy();
            socket.put(0, new Data.ToPhi(this.localhost()));
            socket.put(1, new Data.ToPhi(taken.port()));
            final Phi listen = socket.take("listen").copy();
            listen.put(1, Phi.Φ.take("dataized").copy());
            MatcherAssert.assertThat(
                "the taken port should have told the fallback which address it failed to bind to, but it didnt",
                new Dataized(listen).asString(),
                Matchers.containsString(String.format("%s:%d", this.localhost(), taken.port()))
            );
        } finally {
            taken.stop();
        }
    }

    @Test
    void sendsAndReceivesMessageViaSocketObject() throws InterruptedException, IOException {
        final String msg = "Hello, Socket!";
        final AtomicReference<byte[]> bytes = new AtomicReference<>();
        final RandomServer random = new RandomServer();
        random.stop();
        final int port = random.port();
        final Thread server = new Thread(
            () -> {
                final Phi socket = Phi.Φ.take("socket").copy();
                socket.put(0, new Data.ToPhi(this.localhost()));
                socket.put(1, new Data.ToPhi(port));
                final Phi listened = socket.take("listen").copy();
                listened.put(0, new SyscallTest.Server(msg.length()));
                bytes.set(new Dataized(listened).take());
            }
        );
        server.start();
        Thread.sleep(2000);
        final Phi socket = Phi.Φ.take("socket").copy();
        socket.put(0, new Data.ToPhi(this.localhost()));
        socket.put(1, new Data.ToPhi(port));
        final Phi connected = socket.take("connect").copy();
        connected.put(0, new SyscallTest.Client(msg));
        final int sent = new Dataized(connected).asNumber().intValue();
        server.join();
        MatcherAssert.assertThat(
            String.format(
                "The message had to travel from the client to the server intact, but it didnt, while %d byte(s) were sent",
                sent
            ),
            new String(bytes.get(), StandardCharsets.UTF_8),
            Matchers.equalTo(msg)
        );
    }

    /**
     * Returns the localhost address.
     */
    private String localhost() {
        return "127.0.0.1";
    }

    /**
     * Simple scoped object.
     * true > [s]
     * @since 0.40.0
     */
    private static final class Simple extends PhDefault implements Atom {

        /**
         * Ctor.
         */
        Simple() {
            super(new Attrs(new Attr("s", new AtVoid("s"))));
        }

        @Override
        public Phi lambda() {
            return new Data.ToPhi(true);
        }
    }

    /**
     * Scoped server socket.
     * [s]
     *   s.accept > @
     *     [client]
     *       client.recv 14 > @
     * @since 0.40.0
     */
    private static final class Server extends PhDefault implements Atom {

        /**
         * Received message size.
         */
        private final int received;

        /**
         * Ctor.
         * @param received Reseived message size
         */
        Server(final int received) {
            super(new Attrs(new Attr("s", new AtVoid("s"))));
            this.received = received;
        }

        @Override
        public Phi lambda() {
            final Phi accept = this.take("s").take("accept").copy();
            accept.put(0, new SyscallTest.Receiver(this.received));
            return accept;
        }
    }

    /**
     * Client socket that receives message.
     * s.recv 14 > [s]
     * @since 0.40.0
     */
    private static final class Receiver extends PhDefault implements Atom {

        /**
         * Received message size.
         */
        private final int received;

        /**
         * Ctor.
         * @param received Reseived message size
         */
        Receiver(final int received) {
            super(new Attrs(new Attr("s", new AtVoid("s"))));
            this.received = received;
        }

        @Override
        public Phi lambda() {
            final Phi recv = this.take("s").take("recv");
            recv.put(0, new Data.ToPhi(this.received));
            return recv;
        }
    }

    /**
     * Scoped client socket.
     * s.send "Hello, Socket!" > [s]
     * @since 0.40.0
     */
    private static final class Client extends PhDefault implements Atom {

        /**
         * Message to send.
         */
        private final String message;

        /**
         * Ctor.
         * @param msg Message to send
         */
        Client(final String msg) {
            super(new Attrs(new Attr("s", new AtVoid("s"))));
            this.message = msg;
        }

        @Override
        public Phi lambda() {
            final Phi sent = this.take("s").take("send").copy();
            sent.put(0, new Data.ToPhi(this.message));
            return sent;
        }
    }
}
