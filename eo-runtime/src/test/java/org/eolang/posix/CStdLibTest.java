/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.posix;

import com.jcabi.log.Logger;
import com.sun.jna.Native;
import com.sun.jna.ptr.IntByReference;
import io.github.artsok.RepeatedIfExceptionsTest;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;
import org.eolang.NetworkPort;
import org.eolang.RandomPort;
import org.eolang.RandomServer;
import org.eolang.ReceivedBytes;
import org.eolang.ServerHandoff;
import org.eolang.SockaddrIn;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.condition.DisabledOnOs;
import org.junit.jupiter.api.condition.OS;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;

/**
 * Test case for the {@link CStdLib} syscalls behind the {@code socket} object.
 * @since 0.40
 */
@DisabledOnOs(OS.WINDOWS)
@Execution(ExecutionMode.SAME_THREAD)
final class CStdLibTest {

    /**
     * Backlog for a socket that actually accepts connections.
     */
    private static final int LISTEN_BACKLOG = 5;

    /**
     * Backlog for a socket that only exercises {@code listen} itself.
     */
    private static final int TEST_BACKLOG = 2;

    /**
     * Maximum bind attempts before giving up.
     */
    private static final int MAX_BIND_ATTEMPTS = 50;

    /**
     * How long to wait for a background server thread to finish.
     */
    private static final long JOIN_MILLIS = 5_000L;

    /**
     * How long to wait for a background server thread to start listening.
     */
    private static final long LISTEN_MILLIS = 5_000L;

    @RepeatedIfExceptionsTest(repeats = 3)
    void connectsToLocalServerViaSyscall() throws IOException {
        try (RandomServer server = new RandomServer()) {
            final int socket = this.openSocket();
            try {
                this.ensure(socket > 0);
                final SockaddrIn addr = this.sockaddr(server.port());
                MatcherAssert.assertThat(
                    String.format(
                        "Posix socket should have been connected to local server via syscall, but it didn't, reason: %s",
                        this.getError()
                    ),
                    CStdLib.INSTANCE.connect(socket, addr, addr.size()),
                    Matchers.equalTo(0)
                );
            } finally {
                this.closeSocket(socket);
            }
        }
    }

    @RepeatedIfExceptionsTest(repeats = 3)
    void refusesConnectionViaSyscall() {
        final int socket = this.openSocket();
        try {
            this.ensure(socket > 0);
            final SockaddrIn addr = this.sockaddr(1234);
            MatcherAssert.assertThat(
                "Connection via posix syscall to wrong port must be refused",
                CStdLib.INSTANCE.connect(socket, addr, addr.size()),
                Matchers.equalTo(-1)
            );
        } finally {
            this.closeSocket(socket);
        }
    }

    @RepeatedIfExceptionsTest(repeats = 3)
    void bindsSocketSuccessfullyViaSyscall() {
        final int socket = this.openSocket();
        try {
            this.ensure(socket > 0);
            final int port = new RandomPort().pick();
            MatcherAssert.assertThat(
                String.format(
                    "Posix socket should have been bound to localhost:%d via syscall, but it didn't, reason: %s",
                    port, this.getError()
                ),
                this.bindSocket(socket, port),
                Matchers.equalTo(0)
            );
        } finally {
            this.closeSocket(socket);
        }
    }

    @RepeatedIfExceptionsTest(repeats = 3)
    void startsListenOnPosixSocket() {
        final int socket = this.openSocket();
        try {
            this.ensure(socket > 0);
            this.ensure(this.bindSocket(socket, new RandomPort().pick()) == 0);
            MatcherAssert.assertThat(
                String.format(
                    "Posix socket should have been bound to localhost via syscall, but it didn't, reason: %s",
                    this.getError()
                ),
                CStdLib.INSTANCE.listen(socket, CStdLibTest.TEST_BACKLOG),
                Matchers.equalTo(0)
            );
        } finally {
            this.closeSocket(socket);
        }
    }

    @RepeatedIfExceptionsTest(repeats = 3)
    void acceptsConnectionOnSocket() throws InterruptedException {
        final AtomicInteger accept = new AtomicInteger(0);
        final AtomicReference<String> error = new AtomicReference<>();
        final ServerHandoff handoff = new ServerHandoff(new RandomPort().pick());
        final Thread server = new Thread(
            () -> this.acceptViaCStdLib(handoff, accept, error)
        );
        server.start();
        this.ensure(handoff.awaited(CStdLibTest.LISTEN_MILLIS));
        final int client = this.openSocket();
        try {
            this.ensure(client > 0);
            final SockaddrIn sockaddr = this.sockaddr(handoff.port().get());
            MatcherAssert.assertThat(
                String.format(
                    "Socket should have been connected to local server on sockets, but it didn't, reason: %s",
                    this.getError()
                ),
                CStdLib.INSTANCE.connect(client, sockaddr, sockaddr.size()),
                Matchers.equalTo(0)
            );
            this.joined(server);
            MatcherAssert.assertThat(
                String.format(
                    "Accepted client socket must be positive, but it isn't, reason: %s",
                    error.get()
                ),
                accept.get(),
                Matchers.greaterThan(0)
            );
        } finally {
            this.closeSocket(client);
        }
    }

    @RepeatedIfExceptionsTest(repeats = 3)
    void sendsAndReceivesMessagesViaSyscalls() throws InterruptedException {
        final AtomicInteger received = new AtomicInteger(-1);
        final AtomicReference<byte[]> bytes = new AtomicReference<>();
        final ServerHandoff handoff = new ServerHandoff(new RandomPort().pick());
        final Thread server = new Thread(
            () -> this.recvViaCStdLib(handoff, received, bytes)
        );
        server.start();
        this.ensure(handoff.awaited(CStdLibTest.LISTEN_MILLIS));
        final int client = this.openSocket();
        try {
            this.ensure(client > 0);
            final SockaddrIn sockaddr = this.sockaddr(handoff.port().get());
            this.ensure(CStdLib.INSTANCE.connect(client, sockaddr, sockaddr.size()) == 0);
            final byte[] buf = "Hello, Socket!".getBytes(StandardCharsets.UTF_8);
            final int sent = CStdLib.INSTANCE.send(client, buf, buf.length, 0);
            MatcherAssert.assertThat(
                String.format(
                    "Client had to send %d bytes to the server, but sent %d, reason: %s",
                    buf.length, sent, this.getError()
                ),
                sent,
                Matchers.equalTo(buf.length)
            );
            this.joined(server);
            new ReceivedBytes(buf, received, bytes).verify();
        } finally {
            this.closeSocket(client);
        }
    }

    /**
     * Ensure that the given condition is true, or print last error otherwise.
     * @param condition Condition to check
     */
    private void ensure(final boolean condition) {
        if (!condition) {
            Logger.debug(this, "Strerror: %s", this.getError());
        }
        assert condition;
    }

    /**
     * Open posix socket.
     * @return Posix socket descriptor
     */
    private int openSocket() {
        final int sock = CStdLib.INSTANCE.socket(
            CStdLib.AF_INET,
            CStdLib.SOCK_STREAM,
            CStdLib.IPPROTO_TCP
        );
        Logger.debug(this, "Opened socket: %d", sock);
        return sock;
    }

    /**
     * Close posix socket.
     * @param socket Socket to close
     * @return Zero on success, -1 on error
     */
    private int closeSocket(final int socket) {
        final int closed = CStdLib.INSTANCE.close(socket);
        if (closed == 0) {
            Logger.debug(this, "Closed socket: %d", socket);
        } else {
            Logger.debug(this, "Failed to close socket: %d", socket);
        }
        return closed;
    }

    /**
     * Bind socket.
     * @param socket Socket
     * @param port Port
     * @return Zero on success, -1 on error
     */
    private int bindSocket(final int socket, final int port) {
        final SockaddrIn addr = this.sockaddr(port);
        return CStdLib.INSTANCE.bind(socket, addr, addr.size());
    }

    /**
     * Wait for a server thread to finish, failing fast instead of hanging
     * forever if it never does.
     * @param server Server thread
     * @throws InterruptedException If interrupted while waiting
     */
    private void joined(final Thread server) throws InterruptedException {
        server.join(CStdLibTest.JOIN_MILLIS);
        MatcherAssert.assertThat(
            "Server thread had to finish within the timeout, but it didn't",
            server.isAlive(),
            Matchers.is(false)
        );
    }

    /**
     * Bind a socket to a free port, retrying a bounded number of times so a
     * persistent bind failure fails the test instead of spinning forever.
     * @param socket Socket to bind
     * @param port Port to try first, updated with each retry
     */
    private void bound(final int socket, final AtomicInteger port) {
        int attempt = 0;
        while (this.bindSocket(socket, port.get()) != 0) {
            attempt += 1;
            if (attempt >= CStdLibTest.MAX_BIND_ATTEMPTS) {
                throw new IllegalStateException(
                    String.format(
                        "Could not bind to a free port after %d attempts",
                        CStdLibTest.MAX_BIND_ATTEMPTS
                    )
                );
            }
            port.set(new RandomPort().pick());
        }
    }

    /**
     * Get last posix error.
     * @return Last posix error as string
     */
    private String getError() {
        return CStdLib.INSTANCE.strerror(Native.getLastError());
    }

    /**
     * Call posix inet addr.
     * @param address IP address
     * @return Posix inet addr as integer
     */
    private int inetAddr(final String address) {
        return CStdLib.INSTANCE.inet_addr(address);
    }

    /**
     * Get sockaddr_in structure.
     * @param port Port
     * @return The sockaddr_in structure
     */
    private SockaddrIn sockaddr(final int port) {
        return new SockaddrIn(
            (short) CStdLib.AF_INET,
            new NetworkPort(port).bytes(),
            this.inetAddr("127.0.0.1")
        );
    }

    /**
     * Bind, listen and accept one connection via {@link CStdLib}.
     * @param handoff Port to bind to and the latch signaling the client
     *  when the server is ready
     * @param accept Out-parameter: the accepted socket descriptor
     * @param error Out-parameter: the error string if accept failed
     */
    private void acceptViaCStdLib(
        final ServerHandoff handoff, final AtomicInteger accept,
        final AtomicReference<String> error
    ) {
        final int socket = this.openSocket();
        try {
            this.ensure(socket > 0);
            this.bound(socket, handoff.port());
            this.ensure(CStdLib.INSTANCE.listen(socket, CStdLibTest.LISTEN_BACKLOG) == 0);
            handoff.ready();
            final SockaddrIn addr = new SockaddrIn();
            final int accepted = CStdLib.INSTANCE.accept(
                socket, addr, new IntByReference(addr.size())
            );
            Logger.debug(this, "Accepted socket: %d", accepted);
            accept.set(accepted);
            if (accepted < 0) {
                error.set(this.getError());
            }
        } finally {
            if (accept.get() > 0) {
                this.closeSocket(accept.get());
            }
            this.closeSocket(socket);
        }
    }

    /**
     * Bind, listen, accept one connection and receive a message via
     * {@link CStdLib}.
     * @param handoff Port to bind to and the latch signaling the client
     *  when the server is ready
     * @param received Out-parameter: number of bytes received
     * @param bytes Out-parameter: the bytes received
     */
    private void recvViaCStdLib(
        final ServerHandoff handoff, final AtomicInteger received,
        final AtomicReference<byte[]> bytes
    ) {
        final int socket = this.openSocket();
        int accepted = 0;
        try {
            this.ensure(socket > 0);
            this.bound(socket, handoff.port());
            this.ensure(CStdLib.INSTANCE.listen(socket, CStdLibTest.LISTEN_BACKLOG) == 0);
            handoff.ready();
            final SockaddrIn addr = new SockaddrIn();
            accepted = CStdLib.INSTANCE.accept(
                socket, addr, new IntByReference(addr.size())
            );
            Logger.debug(this, "Accepted socket: %d", accepted);
            this.ensure(accepted > 0);
            final byte[] buf = new byte[1024];
            received.set(CStdLib.INSTANCE.recv(accepted, buf, buf.length, 0));
            bytes.set(Arrays.copyOf(buf, received.get()));
        } finally {
            if (accepted > 0) {
                this.closeSocket(accepted);
            }
            this.closeSocket(socket);
        }
    }
}
