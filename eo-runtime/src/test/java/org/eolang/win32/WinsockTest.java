/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.win32;

import com.jcabi.log.Logger;
import com.sun.jna.ptr.IntByReference;
import io.github.artsok.RepeatedIfExceptionsTest;
import java.io.IOException;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;
import org.eolang.NetworkPort;
import org.eolang.RandomPort;
import org.eolang.RandomServer;
import org.eolang.ReceivedBytes;
import org.eolang.SockaddrIn;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.condition.DisabledOnOs;
import org.junit.jupiter.api.condition.OS;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;

/**
 * Test case for the {@link Winsock} syscalls behind the {@code socket} object.
 * @since 0.40
 */
@DisabledOnOs({OS.MAC, OS.LINUX})
@Execution(ExecutionMode.SAME_THREAD)
final class WinsockTest {

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
            this.ensure(this.startup() == 0);
            final int socket = this.openSocket();
            try {
                this.ensure(socket > 0);
                final SockaddrIn addr = this.sockaddr(server.port());
                MatcherAssert.assertThat(
                    String.format(
                        "Windows socket should have been connected to local server via syscall, but it didn't, error code is: %d",
                        this.getError()
                    ),
                    Winsock.INSTANCE.connect(socket, addr, addr.size()),
                    Matchers.equalTo(0)
                );
            } finally {
                this.closeSocket(socket);
            }
        } finally {
            this.cleanup();
        }
    }

    @RepeatedIfExceptionsTest(repeats = 3)
    void refusesConnectionViaSyscall() throws UnknownHostException {
        try {
            this.ensure(this.startup() == 0);
            final int socket = this.openSocket();
            try {
                this.ensure(socket > 0);
                final SockaddrIn addr = new SockaddrIn(
                    (short) Winsock.AF_INET,
                    new NetworkPort(8080).bytes(),
                    this.inetAddr("192.0.2.1")
                );
                MatcherAssert.assertThat(
                    "Connection via windows syscall to Test-Net (192.0.2.1) must be refused",
                    Winsock.INSTANCE.connect(socket, addr, addr.size()),
                    Matchers.equalTo(-1)
                );
            } finally {
                this.closeSocket(socket);
            }
        } finally {
            this.cleanup();
        }
    }

    @RepeatedIfExceptionsTest(repeats = 3)
    void bindsSocketSuccessfullyViaSyscall() throws UnknownHostException {
        try {
            this.ensure(this.startup() == 0);
            final int socket = this.openSocket();
            try {
                this.ensure(socket > 0);
                final int port = new RandomPort().pick();
                MatcherAssert.assertThat(
                    String.format(
                        "Win socket should have been bound to localhost:%d via syscall, but it didn't, error code is: %d",
                        port, this.getError()
                    ),
                    this.bindSocket(socket, port),
                    Matchers.equalTo(0)
                );
            } finally {
                this.closeSocket(socket);
            }
        } finally {
            this.cleanup();
        }
    }

    @RepeatedIfExceptionsTest(repeats = 3)
    void startsListenOnWindowsSocket() throws UnknownHostException {
        try {
            this.ensure(this.startup() == 0);
            final int socket = this.openSocket();
            try {
                this.ensure(socket > 0);
                this.ensure(this.bindSocket(socket, new RandomPort().pick()) == 0);
                MatcherAssert.assertThat(
                    String.format(
                        "Windows socket should have started listening on localhost via syscall, but it didn't, reason: %s",
                        this.getError()
                    ),
                    Winsock.INSTANCE.listen(socket, WinsockTest.TEST_BACKLOG),
                    Matchers.equalTo(0)
                );
            } finally {
                this.closeSocket(socket);
            }
        } finally {
            this.cleanup();
        }
    }

    @RepeatedIfExceptionsTest(repeats = 3)
    void acceptsConnectionOnSocket() throws InterruptedException, UnknownHostException {
        try {
            this.ensure(this.startup() == 0);
            final AtomicInteger accept = new AtomicInteger(0);
            final AtomicInteger error = new AtomicInteger();
            final AtomicInteger port = new AtomicInteger(new RandomPort().pick());
            final CountDownLatch listening = new CountDownLatch(1);
            final Thread server = new Thread(
                () -> this.acceptViaWinsock(port, accept, error, listening)
            );
            server.start();
            this.ensure(
                listening.await(WinsockTest.LISTEN_MILLIS, TimeUnit.MILLISECONDS)
            );
            final int client = this.openSocket();
            try {
                this.ensure(client > 0);
                final SockaddrIn sockaddr = this.sockaddr(port.get());
                MatcherAssert.assertThat(
                    String.format(
                        "Socket should have been connected to local server on sockets, but it didn't, reason: %s",
                        this.getError()
                    ),
                    Winsock.INSTANCE.connect(client, sockaddr, sockaddr.size()),
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
        } finally {
            this.cleanup();
        }
    }

    @RepeatedIfExceptionsTest(repeats = 3)
    void sendsAndReceivesMessagesViaSyscalls()
        throws InterruptedException, UnknownHostException {
        try {
            this.ensure(this.startup() == 0);
            final AtomicInteger received = new AtomicInteger(-1);
            final AtomicReference<byte[]> bytes = new AtomicReference<>();
            final AtomicInteger port = new AtomicInteger(new RandomPort().pick());
            final CountDownLatch listening = new CountDownLatch(1);
            final Thread server = new Thread(
                () -> this.recvViaWinsock(port, received, bytes, listening)
            );
            server.start();
            this.ensure(
                listening.await(WinsockTest.LISTEN_MILLIS, TimeUnit.MILLISECONDS)
            );
            final int client = this.openSocket();
            try {
                this.ensure(client > 0);
                final SockaddrIn sockaddr = this.sockaddr(port.get());
                this.ensure(Winsock.INSTANCE.connect(client, sockaddr, sockaddr.size()) == 0);
                final byte[] buf = "Hello, Socket!".getBytes(StandardCharsets.UTF_8);
                final int sent = Winsock.INSTANCE.send(client, buf, buf.length, 0);
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
        } finally {
            this.cleanup();
        }
    }

    /**
     * Open socket.
     * @return Socket descriptor
     */
    private int openSocket() {
        final int socket = Winsock.INSTANCE.socket(
            Winsock.AF_INET,
            Winsock.SOCK_STREAM,
            Winsock.IPPROTO_TCP
        );
        Logger.debug(this, "Opened socket: %d", socket);
        return socket;
    }

    /**
     * Close socket.
     * @param socket Socket descriptor
     * @return Zero on success, -1 on error
     */
    private int closeSocket(final int socket) {
        final int closed = Winsock.INSTANCE.closesocket(socket);
        if (closed == 0) {
            Logger.debug(this, "Closed socket: %d", socket);
        } else {
            Logger.debug(this, "Failed to close socket: %d", socket);
        }
        return closed;
    }

    /**
     * Start Winsock DLL.
     * @return Zero on success, -1 on error
     */
    private int startup() {
        return Winsock.INSTANCE.WSAStartup(
            Winsock.WINSOCK_VERSION_2_2, new WSAStartupFuncCall.WSAData()
        );
    }

    /**
     * Cleanup Winsock resources.
     * @return Zero on success, -1 on error
     */
    private int cleanup() {
        return Winsock.INSTANCE.WSACleanup();
    }

    /**
     * Ensure that the given condition is true, or print last error otherwise.
     * @param condition Condition to check
     */
    private void ensure(final boolean condition) {
        if (!condition) {
            Logger.debug(this, "Error code: %d", this.getError());
        }
        assert condition;
    }

    /**
     * Get last Winsock error code.
     * @return Last Winsock error code
     */
    private int getError() {
        return Winsock.INSTANCE.WSAGetLastError();
    }

    /**
     * Bind socket.
     * @param socket Socket
     * @param port Port
     * @return Zero on success, -1 on error
     */
    private int bindSocket(final int socket, final int port) throws UnknownHostException {
        final SockaddrIn addr = this.sockaddr(port);
        return Winsock.INSTANCE.bind(socket, addr, addr.size());
    }

    /**
     * Wait for a server thread to finish, failing fast instead of hanging
     * forever if it never does.
     * @param server Server thread
     * @throws InterruptedException If interrupted while waiting
     */
    private void joined(final Thread server) throws InterruptedException {
        server.join(WinsockTest.JOIN_MILLIS);
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
     * @throws UnknownHostException If the loopback address cannot be resolved
     */
    private void bound(final int socket, final AtomicInteger port) throws UnknownHostException {
        int attempt = 0;
        while (this.bindSocket(socket, port.get()) != 0) {
            attempt += 1;
            if (attempt >= WinsockTest.MAX_BIND_ATTEMPTS) {
                throw new IllegalStateException(
                    String.format(
                        "Could not bind to a free port after %d attempts",
                        WinsockTest.MAX_BIND_ATTEMPTS
                    )
                );
            }
            port.set(new RandomPort().pick());
        }
    }

    /**
     * Call posix inet addr.
     * @param address IP address
     * @return Posix inet addr as integer
     */
    private int inetAddr(final String address) throws UnknownHostException {
        final ByteBuffer buffer = ByteBuffer.allocate(4);
        buffer.put(InetAddress.getByName(address).getAddress());
        return Integer.reverseBytes(buffer.getInt(0));
    }

    /**
     * Get sockaddr_in structure.
     * @param port Port
     * @return The sockaddr_in structure
     */
    private SockaddrIn sockaddr(final int port) throws UnknownHostException {
        return new SockaddrIn(
            (short) Winsock.AF_INET,
            new NetworkPort(port).bytes(),
            this.inetAddr("127.0.0.1")
        );
    }

    /**
     * Bind, listen and accept one connection via {@link Winsock}.
     * @param port Port to bind to, updated if the candidate is taken
     * @param accept Out-parameter: the accepted socket descriptor
     * @param error Out-parameter: the Winsock error code if accept failed
     * @param listening Counted down once listen() succeeds, so the client
     *  does not have to guess when the server is ready
     * @checkstyle ParameterNumberCheck (3 lines)
     */
    private void acceptViaWinsock(
        final AtomicInteger port, final AtomicInteger accept, final AtomicInteger error,
        final CountDownLatch listening
    ) {
        final int socket = this.openSocket();
        try {
            this.ensure(socket > 0);
            this.bound(socket, port);
            this.ensure(Winsock.INSTANCE.listen(socket, WinsockTest.LISTEN_BACKLOG) == 0);
            listening.countDown();
            final SockaddrIn addr = new SockaddrIn();
            final int accepted = Winsock.INSTANCE.accept(
                socket, addr, new IntByReference(addr.size())
            );
            Logger.debug(this, "Accepted socket: %d", accepted);
            accept.set(accepted);
            if (accepted < 0) {
                error.set(this.getError());
            }
        } catch (final UnknownHostException exception) {
            throw new IllegalStateException(exception);
        } finally {
            if (accept.get() > 0) {
                this.closeSocket(accept.get());
            }
            this.closeSocket(socket);
        }
    }

    /**
     * Bind, listen, accept one connection and receive a message via
     * {@link Winsock}.
     * @param port Port to bind to, updated if the candidate is taken
     * @param received Out-parameter: number of bytes received
     * @param bytes Out-parameter: the bytes received
     * @param listening Counted down once listen() succeeds, so the client
     *  does not have to guess when the server is ready
     * @checkstyle ParameterNumberCheck (3 lines)
     */
    private void recvViaWinsock(
        final AtomicInteger port, final AtomicInteger received,
        final AtomicReference<byte[]> bytes, final CountDownLatch listening
    ) {
        final int socket = this.openSocket();
        int accepted = 0;
        try {
            this.ensure(socket > 0);
            this.bound(socket, port);
            this.ensure(Winsock.INSTANCE.listen(socket, WinsockTest.LISTEN_BACKLOG) == 0);
            listening.countDown();
            final SockaddrIn addr = new SockaddrIn();
            accepted = Winsock.INSTANCE.accept(
                socket, addr, new IntByReference(addr.size())
            );
            Logger.debug(this, "Accepted socket: %d", accepted);
            this.ensure(accepted > 0);
            final byte[] buf = new byte[1024];
            received.set(Winsock.INSTANCE.recv(accepted, buf, buf.length, 0));
            bytes.set(Arrays.copyOf(buf, received.get()));
        } catch (final UnknownHostException exception) {
            throw new IllegalStateException(exception);
        } finally {
            if (accepted > 0) {
                this.closeSocket(accepted);
            }
            this.closeSocket(socket);
        }
    }
}
