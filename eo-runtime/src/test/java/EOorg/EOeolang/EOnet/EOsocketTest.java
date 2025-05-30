/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
/*
 * @checkstyle PackageNameCheck (4 lines)
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package EOorg.EOeolang.EOnet; // NOPMD

import EOorg.EOeolang.EOsys.Posix.CStdLib;
import EOorg.EOeolang.EOsys.SockaddrIn;
import EOorg.EOeolang.EOsys.Win32.WSAStartupFuncCall;
import EOorg.EOeolang.EOsys.Win32.Winsock;
import com.jcabi.log.Logger;
import com.sun.jna.Native;
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
import java.util.Random;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;
import org.eolang.Atom;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.PhDefault;
import org.eolang.PhVoid;
import org.eolang.Phi;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.DisabledOnOs;
import org.junit.jupiter.api.condition.OS;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;

/**
 * Test case for {@link EOsocket}.
 * @since 0.40
 * @checkstyle TypeNameCheck (100 lines)
 */
@SuppressWarnings({
    "PMD.TooManyMethods",
    "PMD.AvoidUsingHardCodedIP",
    "PMD.CloseResource",
    "JTCOP.RuleAllTestsHaveProductionClass"
})
@Execution(ExecutionMode.SAME_THREAD)
final class EOsocketTest {
    /**
     * Localhost IP.
     */
    private static final String LOCALHOST = "127.0.0.1";

    /**
     * Random.
     */
    private static final Random RANDOM = new Random();

    @Test
    void connectsToLocalServerViaSocketObject() throws IOException {
        final RandomServer server = new RandomServer().started();
        try {
            final Phi socket = Phi.Φ.take("org.eolang.net.socket").copy();
            socket.put(0, new Data.ToPhi(EOsocketTest.LOCALHOST));
            socket.put(1, new Data.ToPhi(server.port));
            final Phi connected = socket.take("connect").copy();
            connected.put(0, new Simple());
            final byte[] expected = {1};
            final byte[] actual = new Dataized(connected).take();
            MatcherAssert.assertThat(
                String.format(
                    "The 'socket.connect' should have been successfully connected to local server, but it didn't, reason: %s",
                    new String(actual, StandardCharsets.UTF_8)
                ),
                actual,
                Matchers.equalTo(expected)
            );
        } finally {
            server.stop();
        }
    }

    @Test
    void sendsAndReceivesMessageViaSocketObject() throws InterruptedException, IOException {
        final String msg = "Hello, Socket!";
        final AtomicReference<byte[]> bytes = new AtomicReference<>();
        final RandomServer random = new RandomServer().started();
        random.stop();
        final int port = random.port;
        final Thread server = new Thread(
            () -> {
                final Phi socket = Phi.Φ.take("org.eolang.net.socket").copy();
                socket.put(0, new Data.ToPhi(EOsocketTest.LOCALHOST));
                socket.put(1, new Data.ToPhi(port));
                final Phi listened = socket.take("listen").copy();
                listened.put(0, new Server(msg.length()));
                bytes.set(new Dataized(listened).take());
            }
        );
        server.start();
        Thread.sleep(2000);
        final Phi socket = Phi.Φ.take("org.eolang.net.socket").copy();
        socket.put(0, new Data.ToPhi(EOsocketTest.LOCALHOST));
        socket.put(1, new Data.ToPhi(port));
        final Phi connected = socket.take("connect").copy();
        connected.put(0, new Client(msg));
        final int sent = new Dataized(connected).asNumber().intValue();
        server.join();
        MatcherAssert.assertThat(
            "Client had to send message to the server, but it didn't",
            sent,
            Matchers.equalTo(msg.length())
        );
        MatcherAssert.assertThat(
            "Server had to receive message from the client, but it didn't",
            new String(bytes.get(), StandardCharsets.UTF_8),
            Matchers.equalTo(msg)
        );
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
     * Get random port.
     * @return Random port
     */
    private static int randomPort() {
        final int min = 10_000;
        final int max = 20_000;
        return EOsocketTest.RANDOM.nextInt((max - min) + 1) + min;
    }

    /**
     * Winsock tests.
     * @since 0.40.0
     */
    @Nested
    @DisabledOnOs({OS.MAC, OS.LINUX})
    @Execution(ExecutionMode.SAME_THREAD)
    final class WindowsSocketTest {
        @RepeatedIfExceptionsTest(repeats = 3)
        void connectsToLocalServerViaSyscall() throws IOException {
            final RandomServer server = new RandomServer().started();
            final int started = this.startup();
            try {
                this.ensure(started == 0);
                final int socket = this.openSocket();
                try {
                    this.ensure(socket > 0);
                    final SockaddrIn addr = this.sockaddr(server.port);
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
            final int started = this.startup();
            try {
                this.ensure(started == 0);
                final int socket = this.openSocket();
                try {
                    this.ensure(socket > 0);
                    final SockaddrIn addr = new SockaddrIn(
                        (short) Winsock.AF_INET,
                        EOsocketTest.htons(8080),
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
            final int started = this.startup();
            try {
                this.ensure(started == 0);
                final int socket = this.openSocket();
                try {
                    this.ensure(socket > 0);
                    MatcherAssert.assertThat(
                        String.format(
                            "Win socket should have been bound to localhost via syscall, but it didn't, error code is: %d",
                            this.getError()
                        ),
                        this.bindSocket(socket, EOsocketTest.randomPort()),
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
            final int started = this.startup();
            try {
                this.ensure(started == 0);
                final int socket = this.openSocket();
                try {
                    this.ensure(socket > 0);
                    this.ensure(this.bindSocket(socket, EOsocketTest.randomPort()) == 0);
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
        @SuppressWarnings("PMD.AvoidThrowingRawExceptionTypes")
        void acceptsConnectionOnSocket() throws InterruptedException, UnknownHostException {
            final int started = this.startup();
            try {
                this.ensure(started == 0);
                final AtomicInteger accept = new AtomicInteger(0);
                final AtomicInteger error = new AtomicInteger();
                final AtomicInteger port = new AtomicInteger(EOsocketTest.randomPort());
                final Thread server = new Thread(
                    () -> {
                        final int socket = this.openSocket();
                        try {
                            this.ensure(socket > 0);
                            while (this.bindSocket(socket, port.get()) != 0) {
                                port.set(EOsocketTest.randomPort());
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
                            throw new RuntimeException(exception);
                        } finally {
                            if (accept.get() > 0) {
                                this.closeSocket(accept.get());
                            }
                            this.closeSocket(socket);
                        }
                    }
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
        @SuppressWarnings("PMD.AvoidThrowingRawExceptionTypes")
        void sendsAndReceivesMessagesViaSyscalls()
            throws InterruptedException, UnknownHostException {
            final int started = this.startup();
            try {
                this.ensure(started == 0);
                final AtomicInteger received = new AtomicInteger(-1);
                final AtomicReference<byte[]> bytes = new AtomicReference<>();
                final AtomicInteger port = new AtomicInteger(EOsocketTest.randomPort());
                final Thread server = new Thread(
                    () -> {
                        final int socket = this.openSocket();
                        int accepted = 0;
                        try {
                            this.ensure(socket > 0);
                            while (this.bindSocket(socket, port.get()) != 0) {
                                port.set(EOsocketTest.randomPort());
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
                            throw new RuntimeException(exception);
                        } finally {
                            this.closeSocket(accepted);
                            this.closeSocket(socket);
                        }
                    }
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
                            "Client had to sent message to the server, but it didn't, reason: %s",
                            this.getError()
                        ),
                        sent,
                        Matchers.equalTo(buf.length)
                    );
                    server.join();
                    MatcherAssert.assertThat(
                        String.format(
                            "Server hat to receive message from the client, but it didn't, reason: %s",
                            this.getError()
                        ),
                        received.get(),
                        Matchers.equalTo(buf.length)
                    );
                    MatcherAssert.assertThat(
                        "Received bytes must be equal to sent, but they didn't",
                        new String(bytes.get(), StandardCharsets.UTF_8),
                        Matchers.equalTo(new String(buf, StandardCharsets.UTF_8))
                    );
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
            final byte[] bytes = InetAddress.getByName(address).getAddress();
            final ByteBuffer buffer = ByteBuffer.allocate(4);
            buffer.put(bytes);
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
                EOsocketTest.htons(port),
                this.inetAddr(EOsocketTest.LOCALHOST)
            );
        }
    }

    /**
     * Posix socket test.
     * @since 0.40.0
     */
    @Nested
    @DisabledOnOs(OS.WINDOWS)
    @Execution(ExecutionMode.SAME_THREAD)
    final class PosixSocketTest {
        @RepeatedIfExceptionsTest(repeats = 3)
        void connectsToLocalServerViaSyscall() throws IOException {
            final RandomServer server = new RandomServer().started();
            final int socket = this.openSocket();
            try {
                this.ensure(socket > 0);
                final SockaddrIn addr = this.sockaddr(server.port);
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
                final int connected = CStdLib.INSTANCE.connect(socket, addr, addr.size());
                MatcherAssert.assertThat(
                    "Connection via posix syscall to wrong port must be refused",
                    connected,
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
                MatcherAssert.assertThat(
                    String.format(
                        "Posix socket should have been bound to localhost via syscall, but it didn't, reason: %s",
                        this.getError()
                    ),
                    this.bindSocket(socket, EOsocketTest.randomPort()),
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
                this.ensure(this.bindSocket(socket, EOsocketTest.randomPort()) == 0);
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
        void acceptsConnectionOnSocket() throws InterruptedException {
            final AtomicInteger accept = new AtomicInteger(0);
            final AtomicReference<String> error = new AtomicReference<>();
            final AtomicInteger port = new AtomicInteger(EOsocketTest.randomPort());
            final Thread server = new Thread(
                () -> {
                    final int socket = this.openSocket();
                    try {
                        this.ensure(socket > 0);
                        while (this.bindSocket(socket, port.get()) != 0) {
                            port.set(EOsocketTest.randomPort());
                        }
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
        void sendsAndReceivesMessagesViaSyscalls() throws InterruptedException {
            final AtomicInteger received = new AtomicInteger(-1);
            final AtomicReference<byte[]> bytes = new AtomicReference<>();
            final AtomicInteger port = new AtomicInteger(EOsocketTest.randomPort());
            final Thread server = new Thread(
                () -> {
                    final int socket = this.openSocket();
                    int accepted = 0;
                    try {
                        this.ensure(socket > 0);
                        while (this.bindSocket(socket, port.get()) != 0) {
                            port.set(EOsocketTest.randomPort());
                        }
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
            );
            server.start();
            Thread.sleep(2000);
            final int client = this.openSocket();
            try {
                this.ensure(client >= 0);
                final SockaddrIn sockaddr = this.sockaddr(port.get());
                this.ensure(CStdLib.INSTANCE.connect(client, sockaddr, sockaddr.size()) == 0);
                final byte[] buf = "Hello, Socket!".getBytes(StandardCharsets.UTF_8);
                final int sent = CStdLib.INSTANCE.send(client, buf, buf.length, 0);
                MatcherAssert.assertThat(
                    String.format(
                        "Client had to sent message to the server, but it didn't, reason: %s",
                        this.getError()
                    ),
                    sent,
                    Matchers.equalTo(buf.length)
                );
                server.join();
                MatcherAssert.assertThat(
                    String.format(
                        "Server hat to receive message from the client, but it didn't, reason: %s",
                        this.getError()
                    ),
                    received.get(),
                    Matchers.equalTo(buf.length)
                );
                MatcherAssert.assertThat(
                    "Received bytes must be equal to sent, but they didn't",
                    new String(bytes.get(), StandardCharsets.UTF_8),
                    Matchers.equalTo(new String(buf, StandardCharsets.UTF_8))
                );
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
         * @return Posix socket descriptor.
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
            return CStdLib.INSTANCE.bind(
                socket,
                this.sockaddr(port),
                16
            );
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
                EOsocketTest.htons(port),
                this.inetAddr(EOsocketTest.LOCALHOST)
            );
        }
    }

    /**
     * Server on random port.
     * @since 0.40.0
     */
    private static final class RandomServer {
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
         * @return Self.
         */
        RandomServer started() {
            boolean bound = false;
            while (!bound) {
                this.port = EOsocketTest.randomPort();
                try {
                    this.socket = new ServerSocket();
                    this.socket.setReuseAddress(true);
                    this.socket.bind(new InetSocketAddress(EOsocketTest.LOCALHOST, this.port));
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
        @SuppressWarnings("PMD.ConstructorOnlyInitializesOrCallOtherConstructors")
        Simple() {
            this.add("s", new PhVoid("s"));
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
        @SuppressWarnings("PMD.ConstructorOnlyInitializesOrCallOtherConstructors")
        Server(final int received) {
            this.add("s", new PhVoid("s"));
            this.received = received;
        }

        @Override
        public Phi lambda() {
            final Phi accept = this.take("s").take("accept").copy();
            accept.put(0, new Receiver(this.received));
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
        @SuppressWarnings("PMD.ConstructorOnlyInitializesOrCallOtherConstructors")
        Receiver(final int received) {
            this.add("s", new PhVoid("s"));
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
        @SuppressWarnings("PMD.ConstructorOnlyInitializesOrCallOtherConstructors")
        Client(final String msg) {
            this.add("s", new PhVoid("s"));
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
