/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import codes.ivanov.ephpo.Ephemeral;
import codes.ivanov.ephpo.EphemeralResolver;
import com.jcabi.log.Logger;
import com.sun.jna.Native;
import com.sun.jna.Pointer;
import com.sun.jna.ptr.IntByReference;
import io.github.artsok.RepeatedIfExceptionsTest;
import java.io.IOException;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.ServerSocket;
import java.net.UnknownHostException;
import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;
import org.eolang.posix.CStdLib;
import org.eolang.win32.WSAData;
import org.eolang.win32.Winsock;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.DisabledOnOs;
import org.junit.jupiter.api.condition.OS;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;

/**
 * Test case for the {@link Syscall} implementations behind the
 * {@code socket} object, both the POSIX and the Windows ones.
 * @since 0.40
 */
@ExtendWith(EphemeralResolver.class)
final class SyscallTest {

    @Test
    void connectsToLocalServerViaSocketObject(@Ephemeral final int port) throws IOException {
        final SyscallTest.RandomServer server = new SyscallTest.RandomServer(port).started();
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
                Matchers.equalTo(new byte[]{(byte) 0xFF})
            );
        } finally {
            server.stop();
        }
    }

    @Test
    void returnsFallbackWhenConnectionIsRefused(@Ephemeral final int port) {
        final Phi socket = Phi.Φ.take("socket").copy();
        socket.put(0, new Data.ToPhi(this.localhost()));
        socket.put(1, new Data.ToPhi(port));
        final Phi connect = socket.take("connect").copy();
        connect.put(0, new SyscallTest.Simple());
        connect.put(1, new SyscallTest.Simple());
        MatcherAssert.assertThat(
            "connecting to a refused port should have yielded the cant-connect fallback instead of terminating, but it didnt",
            new Dataized(connect).take(),
            Matchers.equalTo(new byte[]{(byte) 0xFF})
        );
    }

    @Test
    void tellsTheFallbackWhichAddressItFailedToReach(@Ephemeral final int port) {
        final Phi socket = Phi.Φ.take("socket").copy();
        socket.put(0, new Data.ToPhi(this.localhost()));
        socket.put(1, new Data.ToPhi(port));
        final Phi connect = socket.take("connect").copy();
        connect.put(1, Phi.Φ.take("dataized").copy());
        MatcherAssert.assertThat(
            "the refused connection should have told the fallback which address it failed to reach, but it didnt",
            new Dataized(connect).asString(),
            Matchers.containsString(String.format("%s:%d", this.localhost(), port))
        );
    }

    @Test
    void acceptsTheBroadcastAddressAsAValidConversion(@Ephemeral final int port) {
        final Phi socket = Phi.Φ.take("socket").copy();
        socket.put(0, new Data.ToPhi("255.255.255.255"));
        socket.put(1, new Data.ToPhi(port));
        final Phi listen = socket.take("listen").copy();
        listen.put(0, new SyscallTest.Simple());
        listen.put(1, Phi.Φ.take("dataized").copy());
        MatcherAssert.assertThat(
            "the limited-broadcast address 255.255.255.255 must be accepted as a valid IPv4 conversion, not rejected as unparsable",
            new String(new Dataized(listen).take(), StandardCharsets.UTF_8),
            Matchers.not(Matchers.containsString("into a 32-bit integer"))
        );
    }

    @Test
    void tellsTheFallbackWhichAddressItFailedToBind(@Ephemeral final int port) throws IOException {
        final SyscallTest.RandomServer taken = new SyscallTest.RandomServer(port).started();
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

    @RepeatedIfExceptionsTest(repeats = 3)
    void sendsAndReceivesMessageViaSocketObject(@Ephemeral final int port)
        throws InterruptedException {
        final String msg = "Hello, Socket!";
        final AtomicReference<byte[]> bytes = new AtomicReference<>();
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

    private String localhost() {
        return InetAddress.getLoopbackAddress().getHostAddress();
    }

    private static short htons(final int port) {
        return (short) (((port & 0xFF) << 8) | ((port >> 8) & 0xFF));
    }

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
     * Winsock tests.
     * @since 0.40.0
     */
    @Nested
    @DisabledOnOs({OS.MAC, OS.LINUX})
    @Execution(ExecutionMode.SAME_THREAD)
    @SuppressWarnings("PMD.TestClassWithoutTestCases")
    final class WindowsSocketTest {

        @RepeatedIfExceptionsTest(repeats = 3)
        void connectsToLocalServerViaSyscall(@Ephemeral final int port) throws IOException {
            final SyscallTest.RandomServer server = new SyscallTest.RandomServer(port).started();
            try {
                this.ensure(this.startup() == 0);
                final long socket = this.openSocket();
                try {
                    this.ensure(socket > 0);
                    final SockaddrIn addr = this.sockaddr(server.port());
                    MatcherAssert.assertThat(
                        String.format(
                            "Windows socket should have been connected to local server via syscall, but it didn't, error code is: %d",
                            this.getError()
                        ),
                        Winsock.INSTANCE.connect(new Pointer(socket), addr, addr.size()),
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
        void refusesConnectionViaSyscall(@Ephemeral final int port) throws UnknownHostException {
            try {
                this.ensure(this.startup() == 0);
                final long socket = this.openSocket();
                try {
                    this.ensure(socket > 0);
                    final SockaddrIn addr = this.sockaddr(port);
                    MatcherAssert.assertThat(
                        "Connection via windows syscall to a loopback port nobody listens on must be refused",
                        Winsock.INSTANCE.connect(new Pointer(socket), addr, addr.size()),
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
        void bindsSocketSuccessfullyViaSyscall(@Ephemeral final int port)
            throws UnknownHostException {
            try {
                this.ensure(this.startup() == 0);
                final long socket = this.openSocket();
                try {
                    this.ensure(socket > 0);
                    MatcherAssert.assertThat(
                        String.format(
                            "Win socket should have been bound to localhost via syscall, but it didn't, error code is: %d",
                            this.getError()
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
        void startsListenOnPosixSocket(@Ephemeral final int port)
            throws UnknownHostException {
            try {
                this.ensure(this.startup() == 0);
                final long socket = this.openSocket();
                try {
                    this.ensure(socket > 0);
                    this.ensure(this.bindSocket(socket, port) == 0);
                    MatcherAssert.assertThat(
                        String.format(
                            "Posix socket should have been bound to localhost via syscall, but it didn't, reason: %s",
                            this.getError()
                        ),
                        Winsock.INSTANCE.listen(new Pointer(socket), 2),
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
        void acceptsConnectionOnSocket(@Ephemeral final int port)
            throws InterruptedException, UnknownHostException {
            try {
                this.ensure(this.startup() == 0);
                final AtomicInteger accept = new AtomicInteger(0);
                final AtomicInteger error = new AtomicInteger();
                final Thread server = new Thread(
                    () -> this.acceptViaWinsock(port, accept, error)
                );
                server.start();
                Thread.sleep(2000);
                final long client = this.openSocket();
                try {
                    this.ensure(client >= 0);
                    final SockaddrIn sockaddr = this.sockaddr(port);
                    MatcherAssert.assertThat(
                        String.format(
                            "Socket should have been connected to local server on sockets, but it didn't, reason: %s",
                            this.getError()
                        ),
                        Winsock.INSTANCE.connect(new Pointer(client), sockaddr, sockaddr.size()),
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
        void sendsAndReceivesMessagesViaSyscalls(@Ephemeral final int port)
            throws InterruptedException, UnknownHostException {
            try {
                this.ensure(this.startup() == 0);
                final AtomicInteger received = new AtomicInteger(-1);
                final AtomicReference<byte[]> bytes = new AtomicReference<>();
                final Thread server = new Thread(
                    () -> this.recvViaWinsock(port, received, bytes)
                );
                server.start();
                Thread.sleep(2000);
                final long client = this.openSocket();
                try {
                    this.ensure(client >= 0);
                    final SockaddrIn sockaddr = this.sockaddr(port);
                    this.ensure(
                        Winsock.INSTANCE.connect(new Pointer(client), sockaddr, sockaddr.size())
                            == 0
                    );
                    final byte[] buf = "Hello, Socket!".getBytes(StandardCharsets.UTF_8);
                    final int sent = Winsock.INSTANCE.send(new Pointer(client), buf, buf.length, 0);
                    MatcherAssert.assertThat(
                        String.format(
                            "Client had to send %d bytes to the server, but sent %d, reason: %s",
                            buf.length, sent, this.getError()
                        ),
                        sent,
                        Matchers.equalTo(buf.length)
                    );
                    server.join();
                    SyscallTest.assertReceived(buf, received, bytes);
                } finally {
                    this.closeSocket(client);
                }
            } finally {
                this.cleanup();
            }
        }

        private long openSocket() {
            final long socket = Pointer.nativeValue(
                Winsock.INSTANCE.socket(
                    Winsock.AF_INET,
                    Winsock.SOCK_STREAM,
                    Winsock.IPPROTO_TCP
                )
            );
            Logger.debug(this, "Opened socket: %d", socket);
            return socket;
        }

        private int closeSocket(final long socket) {
            final int closed = Winsock.INSTANCE.closesocket(new Pointer(socket));
            if (closed == 0) {
                Logger.debug(this, "Closed socket: %d", socket);
            } else {
                Logger.debug(this, "Failed to close socket: %d", socket);
            }
            return closed;
        }

        private int startup() {
            return Winsock.INSTANCE.WSAStartup(
                Winsock.VERSION_2_2, new WSAData()
            );
        }

        private int cleanup() {
            return Winsock.INSTANCE.WSACleanup();
        }

        private void ensure(final boolean condition) {
            if (!condition) {
                Logger.debug(this, "Error code: %d", this.getError());
            }
            assert condition;
        }

        private int getError() {
            return Native.getLastError();
        }

        private int bindSocket(final long socket, final int port) throws UnknownHostException {
            return Winsock.INSTANCE.bind(
                new Pointer(socket),
                this.sockaddr(port),
                16
            );
        }

        private int inetAddr(final String address) throws UnknownHostException {
            final ByteBuffer buffer = ByteBuffer.allocate(4);
            buffer.put(InetAddress.getByName(address).getAddress());
            return Integer.reverseBytes(buffer.getInt(0));
        }

        private SockaddrIn sockaddr(final int port) throws UnknownHostException {
            return new SockaddrIn(
                (short) Winsock.AF_INET,
                SyscallTest.htons(port),
                this.inetAddr(InetAddress.getLoopbackAddress().getHostAddress())
            );
        }

        private void acceptViaWinsock(
            final int port, final AtomicInteger accept, final AtomicInteger error
        ) {
            final long socket = this.openSocket();
            try {
                this.ensure(socket > 0);
                this.ensure(this.bindSocket(socket, port) == 0);
                this.ensure(Winsock.INSTANCE.listen(new Pointer(socket), 5) == 0);
                final SockaddrIn addr = new SockaddrIn();
                final long accepted = Pointer.nativeValue(
                    Winsock.INSTANCE.accept(
                        new Pointer(socket), addr, new IntByReference(addr.size())
                    )
                );
                Logger.debug(this, "Accepted socket: %d", accepted);
                accept.set((int) accepted);
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
            final int port, final AtomicInteger received,
            final AtomicReference<byte[]> bytes
        ) {
            final long socket = this.openSocket();
            long accepted = 0L;
            try {
                this.ensure(socket > 0);
                this.ensure(this.bindSocket(socket, port) == 0);
                this.ensure(Winsock.INSTANCE.listen(new Pointer(socket), 5) == 0);
                final SockaddrIn addr = new SockaddrIn();
                accepted = Pointer.nativeValue(
                    Winsock.INSTANCE.accept(
                        new Pointer(socket), addr, new IntByReference(addr.size())
                    )
                );
                Logger.debug(this, "Accepted socket: %d", accepted);
                this.ensure(accepted > 0);
                final byte[] buf = new byte[1024];
                received.set(Winsock.INSTANCE.recv(new Pointer(accepted), buf, buf.length, 0));
                bytes.set(Arrays.copyOf(buf, received.get()));
            } catch (final UnknownHostException exception) {
                throw new IllegalStateException(exception);
            } finally {
                this.closeSocket(accepted);
                this.closeSocket(socket);
            }
        }
    }

    /**
     * Posix socket test.
     * @since 0.40.0
     */
    @Nested
    @DisabledOnOs(OS.WINDOWS)
    @Execution(ExecutionMode.SAME_THREAD)
    @SuppressWarnings("PMD.TestClassWithoutTestCases")
    final class PosixSocketTest {

        @RepeatedIfExceptionsTest(repeats = 3)
        void connectsToLocalServerViaSyscall(@Ephemeral final int port) throws IOException {
            final SyscallTest.RandomServer server = new SyscallTest.RandomServer(port).started();
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
                server.stop();
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
        void bindsSocketSuccessfullyViaSyscall(@Ephemeral final int port) {
            final int socket = this.openSocket();
            try {
                this.ensure(socket > 0);
                MatcherAssert.assertThat(
                    String.format(
                        "Posix socket should have been bound to localhost via syscall, but it didn't, reason: %s",
                        this.getError()
                    ),
                    this.bindSocket(socket, port),
                    Matchers.equalTo(0)
                );
            } finally {
                this.closeSocket(socket);
            }
        }

        @RepeatedIfExceptionsTest(repeats = 3)
        void startsListenOnPosixSocket(@Ephemeral final int port) {
            final int socket = this.openSocket();
            try {
                this.ensure(socket > 0);
                this.ensure(this.bindSocket(socket, port) == 0);
                MatcherAssert.assertThat(
                    String.format(
                        "Posix socket should have been bound to localhost via syscall, but it didn't, reason: %s",
                        this.getError()
                    ),
                    CStdLib.INSTANCE.listen(socket, 2),
                    Matchers.equalTo(0)
                );
            } finally {
                this.closeSocket(socket);
            }
        }

        @RepeatedIfExceptionsTest(repeats = 3)
        void acceptsConnectionOnSocket(@Ephemeral final int port) throws InterruptedException {
            final AtomicInteger accept = new AtomicInteger(0);
            final AtomicReference<String> error = new AtomicReference<>();
            final Thread server = new Thread(
                () -> this.acceptViaCStdLib(port, accept, error)
            );
            server.start();
            Thread.sleep(2000);
            final int client = this.openSocket();
            try {
                this.ensure(client >= 0);
                final SockaddrIn sockaddr = this.sockaddr(port);
                MatcherAssert.assertThat(
                    String.format(
                        "Socket should have been connected to local server on sockets, but it didn't, reason: %s",
                        this.getError()
                    ),
                    CStdLib.INSTANCE.connect(client, sockaddr, sockaddr.size()),
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
        }

        @RepeatedIfExceptionsTest(repeats = 3)
        void sendsAndReceivesMessagesViaSyscalls(@Ephemeral final int port)
            throws InterruptedException {
            final AtomicInteger received = new AtomicInteger(-1);
            final AtomicReference<byte[]> bytes = new AtomicReference<>();
            final Thread server = new Thread(
                () -> this.recvViaCStdLib(port, received, bytes)
            );
            server.start();
            Thread.sleep(2000);
            final int client = this.openSocket();
            try {
                this.ensure(client >= 0);
                final SockaddrIn sockaddr = this.sockaddr(port);
                this.ensure(CStdLib.INSTANCE.connect(client, sockaddr, sockaddr.size()) == 0);
                final byte[] buf = "Hello, Socket!".getBytes(StandardCharsets.UTF_8);
                MatcherAssert.assertThat(
                    String.format(
                        "Client had to sent message to the server, but it didn't, reason: %s",
                        this.getError()
                    ),
                    CStdLib.INSTANCE.send(client, buf, buf.length, 0),
                    Matchers.equalTo(buf.length)
                );
                server.join();
                SyscallTest.assertReceived(buf, received, bytes);
            } finally {
                this.closeSocket(client);
            }
        }

        private void ensure(final boolean condition) {
            if (!condition) {
                Logger.debug(this, "Strerror: %s", this.getError());
            }
            assert condition;
        }

        private int openSocket() {
            final int sock = CStdLib.INSTANCE.socket(
                CStdLib.AF_INET,
                CStdLib.SOCK_STREAM,
                CStdLib.IPPROTO_TCP
            );
            Logger.debug(this, "Opened socket: %d", sock);
            return sock;
        }

        private int closeSocket(final int socket) {
            final int closed = CStdLib.INSTANCE.close(socket);
            if (closed == 0) {
                Logger.debug(this, "Closed socket: %d", socket);
            } else {
                Logger.debug(this, "Failed to close socket: %d", socket);
            }
            return closed;
        }

        private int bindSocket(final int socket, final int port) {
            return CStdLib.INSTANCE.bind(
                socket,
                this.sockaddr(port),
                16
            );
        }

        private String getError() {
            return CStdLib.INSTANCE.strerror(Native.getLastError());
        }

        private int inetAddr(final String address) {
            return CStdLib.INSTANCE.inet_addr(address);
        }

        private SockaddrIn sockaddr(final int port) {
            return new SockaddrIn(
                (short) CStdLib.AF_INET,
                SyscallTest.htons(port),
                this.inetAddr(InetAddress.getLoopbackAddress().getHostAddress())
            );
        }

        private void acceptViaCStdLib(
            final int port, final AtomicInteger accept,
            final AtomicReference<String> error
        ) {
            final int socket = this.openSocket();
            try {
                this.ensure(socket > 0);
                this.ensure(this.bindSocket(socket, port) == 0);
                this.ensure(CStdLib.INSTANCE.listen(socket, 5) == 0);
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

        private void recvViaCStdLib(
            final int port, final AtomicInteger received,
            final AtomicReference<byte[]> bytes
        ) {
            final int socket = this.openSocket();
            int accepted = 0;
            try {
                this.ensure(socket > 0);
                this.ensure(this.bindSocket(socket, port) == 0);
                this.ensure(CStdLib.INSTANCE.listen(socket, 5) == 0);
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
                this.closeSocket(accepted);
                this.closeSocket(socket);
            }
        }
    }

    /**
     * Server on a given port.
     * @since 0.40.0
     */
    private static final class RandomServer {

        /**
         * Port to bind to.
         */
        private final int port;

        /**
         * Server socket.
         */
        private ServerSocket socket;

        /**
         * Ctor.
         * @param port Port to bind to
         */
        RandomServer(final int port) {
            this.port = port;
        }

        /**
         * Port the server is bound to.
         * @return Port number
         */
        int port() {
            return this.port;
        }

        /**
         * Start server on the given port.
         * @return Self
         */
        RandomServer started() throws IOException {
            this.socket = new ServerSocket();
            this.socket.setReuseAddress(true);
            this.socket.bind(new InetSocketAddress(InetAddress.getLoopbackAddress(), this.port));
            Logger.debug(this, "Server started on port %d", this.port);
            return this;
        }

        /**
         * Close server socket.
         */
        void stop() throws IOException {
            if (this.socket != null && !this.socket.isClosed()) {
                this.socket.close();
            }
        }
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
     * s.accept > @
     * [client]
     * client.recv 14 > @
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
