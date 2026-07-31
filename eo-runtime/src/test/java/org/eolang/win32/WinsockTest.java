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
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;
import org.eolang.RandomPort;
import org.eolang.RandomServer;
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

    @RepeatedIfExceptionsTest(repeats = 3)
    void connectsToLocalServerViaSyscall() throws IOException {
        final RandomServer server = new RandomServer();
        try {
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
            server.stop();
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
                    WinsockTest.htons(8080),
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
    void startsListenOnPosixSocket() throws UnknownHostException {
        try {
            this.ensure(this.startup() == 0);
            final int socket = this.openSocket();
            try {
                this.ensure(socket > 0);
                this.ensure(this.bindSocket(socket, new RandomPort().pick()) == 0);
                MatcherAssert.assertThat(
                    String.format(
                        "Posix socket should have been bound to localhost via syscall, but it didn't, reason: %s",
                        this.getError()
                    ),
                    Winsock.INSTANCE.listen(socket, 2),
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
            final Thread server = new Thread(
                () -> this.acceptViaWinsock(port, accept, error)
            );
            server.start();
            Thread.sleep(2000);
            final int client = this.openSocket();
            try {
                this.ensure(client >= 0);
                final SockaddrIn sockaddr = this.sockaddr(port.get());
                MatcherAssert.assertThat(
                    String.format(
                        "Socket should have been connected to local server on sockets, but it didn't, reason: %s",
                        this.getError()
                    ),
                    Winsock.INSTANCE.connect(client, sockaddr, sockaddr.size()),
                    Matchers.equalTo(0)
                );
                server.join();
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
            final Thread server = new Thread(
                () -> this.recvViaWinsock(port, received, bytes)
            );
            server.start();
            Thread.sleep(2000);
            final int client = this.openSocket();
            try {
                this.ensure(client >= 0);
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
                server.join();
                WinsockTest.assertReceived(buf, received, bytes);
            } finally {
                this.closeSocket(client);
            }
        } finally {
            this.cleanup();
        }
    }

    /**
     * Convert port number from host to network byte order (htons).
     * @param port Port number
     * @return Port number in network byte order
     */
    private static short htons(final int port) {
        return (short) (((port & 0xFF) << 8) | ((port >> 8) & 0xFF));
    }

    /**
     * Assert that a server thread received exactly the bytes a client sent.
     * @param sent Bytes the client sent
     * @param count Number of bytes the server reported as received
     * @param received Bytes the server received
     */
    private static void assertReceived(
        final byte[] sent, final AtomicInteger count, final AtomicReference<byte[]> received
    ) {
        MatcherAssert.assertThat(
            "Server had to receive the message from the client, but it didn't",
            count.get(),
            Matchers.equalTo(sent.length)
        );
        MatcherAssert.assertThat(
            "Received bytes must be equal to sent, but they didn't",
            new String(received.get(), StandardCharsets.UTF_8),
            Matchers.equalTo(new String(sent, StandardCharsets.UTF_8))
        );
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
        return Winsock.INSTANCE.bind(
            socket,
            this.sockaddr(port),
            16
        );
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
            WinsockTest.htons(port),
            this.inetAddr("127.0.0.1")
        );
    }

    private void acceptViaWinsock(
        final AtomicInteger port, final AtomicInteger accept, final AtomicInteger error
    ) {
        final int socket = this.openSocket();
        try {
            this.ensure(socket > 0);
            while (this.bindSocket(socket, port.get()) != 0) {
                port.set(new RandomPort().pick());
            }
            this.ensure(Winsock.INSTANCE.listen(socket, 5) == 0);
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

    private void recvViaWinsock(
        final AtomicInteger port, final AtomicInteger received,
        final AtomicReference<byte[]> bytes
    ) {
        final int socket = this.openSocket();
        int accepted = 0;
        try {
            this.ensure(socket > 0);
            while (this.bindSocket(socket, port.get()) != 0) {
                port.set(new RandomPort().pick());
            }
            this.ensure(Winsock.INSTANCE.listen(socket, 5) == 0);
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
