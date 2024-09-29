/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2024 Objectionary.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
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
import com.sun.jna.Native;
import java.io.IOException;
import java.net.ServerSocket;
import java.nio.charset.StandardCharsets;
import org.eolang.AtVoid;
import org.eolang.Atom;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.PhDefault;
import org.eolang.Phi;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
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
    "JTCOP.RuleAllTestsHaveProductionClass",
    "PMD.AvoidUsingHardCodedIP",
    "PMD.CloseResource"
})
@Execution(ExecutionMode.SAME_THREAD)
final class EOsocketTest {
    /**
     * Localhost IP.
     */
    private static final String LOCALHOST = "127.0.0.1";

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void connectsToLocalServerViaPosixSyscall() throws IOException {
        final ServerSocket server = EOsocketTest.startServer();
        final int socket = EOsocketTest.openPosixSocket();
        assert socket >= 0;
        final SockaddrIn addr = new SockaddrIn(
            (short) CStdLib.AF_INET,
            EOsocketTest.htons(8080),
            EOsocketTest.posixInetAddr(EOsocketTest.LOCALHOST)
        );
        final int connected = CStdLib.INSTANCE.connect(socket, addr, addr.size());
        final String error;
        if (connected == 0) {
            error = "";
        } else {
            error = EOsocketTest.getPosixError();
        }
        MatcherAssert.assertThat(
            String.format(
                "Posix socket should have been connected to local server via syscall, but it didn't, error is: %s",
                error
            ),
            connected,
            Matchers.equalTo(0)
        );
        assert EOsocketTest.closePosixSocket(socket) == 0;
        EOsocketTest.stopServer(server);
    }

    @Test
    @DisabledOnOs({OS.LINUX, OS.MAC, OS.AIX})
    void connectsToLocalServerViaWindowsSyscall() throws IOException {
        final ServerSocket server = EOsocketTest.startServer();
        assert Winsock.INSTANCE.WSAStartup(
            Winsock.WINSOCK_VERSION_2_2,
            new WSAStartupFuncCall.WSAData()
        ) == 0;
        final int socket = Winsock.INSTANCE.socket(
            Winsock.AF_INET,
            Winsock.SOCK_STREAM,
            Winsock.IPPROTO_TCP
        );
        assert socket >= 0;
        final SockaddrIn addr = new SockaddrIn(
            (short) Winsock.AF_INET,
            EOsocketTest.htons(8080),
            Integer.reverseBytes(Winsock.INSTANCE.inet_addr(EOsocketTest.LOCALHOST))
        );
        final int connected = Winsock.INSTANCE.connect(socket, addr, addr.size());
        final int error;
        if (connected == 0) {
            error = -1;
        } else {
            error = Winsock.INSTANCE.WSAGetLastError();
        }
        MatcherAssert.assertThat(
            String.format(
                "Windows socket should have been connected to local server via syscall, but it didn't, error code is: %d",
                error
            ),
            connected,
            Matchers.equalTo(0)
        );
        assert Winsock.INSTANCE.closesocket(socket) == 0;
        assert Winsock.INSTANCE.WSACleanup() == 0;
        EOsocketTest.stopServer(server);
    }

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void refusesConnectionViaPosixSyscall() {
        final int socket = EOsocketTest.openPosixSocket();
        assert socket >= 0;
        final SockaddrIn addr = new SockaddrIn(
            (short) CStdLib.AF_INET,
            EOsocketTest.htons(1234),
            EOsocketTest.posixInetAddr(EOsocketTest.LOCALHOST)
        );
        final int connected = CStdLib.INSTANCE.connect(socket, addr, addr.size());
        MatcherAssert.assertThat(
            "Connection via posix syscall to wrong port must be refused",
            connected,
            Matchers.equalTo(-1)
        );
        assert EOsocketTest.closePosixSocket(socket) == 0;
    }

    @Test
    @DisabledOnOs({OS.LINUX, OS.MAC, OS.AIX})
    void refusesConnectionViaWindowsSyscall() {
        assert Winsock.INSTANCE.WSAStartup(
            Winsock.WINSOCK_VERSION_2_2,
            new WSAStartupFuncCall.WSAData()
        ) == 0;
        final int socket = Winsock.INSTANCE.socket(
            Winsock.AF_INET,
            Winsock.SOCK_STREAM,
            Winsock.IPPROTO_TCP
        );
        assert socket >= 0;
        final SockaddrIn addr = new SockaddrIn(
            (short) Winsock.AF_INET,
            EOsocketTest.htons(1234),
            Integer.reverseBytes(Winsock.INSTANCE.inet_addr(EOsocketTest.LOCALHOST))
        );
        final int connected = Winsock.INSTANCE.connect(socket, addr, addr.size());
        MatcherAssert.assertThat(
            "Connection via windows syscall to wrong port must be refused",
            connected,
            Matchers.equalTo(-1)
        );
        assert Winsock.INSTANCE.closesocket(socket) == 0;
        assert Winsock.INSTANCE.WSACleanup() == 0;
    }

    @Test
    void connectsToLocalServerViaSocketObject() throws IOException {
        final ServerSocket server = EOsocketTest.startServer();
        final Phi socket = Phi.Φ.take("org.eolang.net.socket").copy();
        socket.put(0, new Data.ToPhi(EOsocketTest.LOCALHOST));
        socket.put(1, new Data.ToPhi(8080));
        final Phi connected = socket.take("connect").copy();
        connected.put(0, new Scoped());
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
        EOsocketTest.stopServer(server);
    }

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void bindsSocketSuccessfullyViaPosixSyscall() {
        final int socket = EOsocketTest.openPosixSocket();
        assert socket >= 0;
        final int bound = EOsocketTest.bindPosixSocket(socket);
        final String error;
        if (bound == 0) {
            error = "";
        } else {
            error = EOsocketTest.getPosixError();
        }
        MatcherAssert.assertThat(
            String.format(
                "Posix socket should have been bound to localhost via syscall, but it didn't, error is: %s",
                error
            ),
            bound,
            Matchers.equalTo(0)
        );
        assert EOsocketTest.closePosixSocket(socket) == 0;
    }

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void startsListenOnPosixSocket() {
        final int socket = EOsocketTest.openPosixSocket();
        assert socket >= 0;
        assert EOsocketTest.bindPosixSocket(socket) == 0;
        final int listened = CStdLib.INSTANCE.listen(socket, 2);
        final String error;
        if (listened == 0) {
            error = "";
        } else {
            error = EOsocketTest.getPosixError();
        }
        MatcherAssert.assertThat(
            String.format(
                "Posix socket should have been bound to localhost via syscall, but it didn't, error is: %s",
                error
            ),
            listened,
            Matchers.equalTo(0)
        );
        assert EOsocketTest.closePosixSocket(socket) == 0;
    }

    /**
     * Open posix socket.
     * @return Posix socket descriptor.
     */
    private static int openPosixSocket() {
        return CStdLib.INSTANCE.socket(
            CStdLib.AF_INET,
            CStdLib.SOCK_STREAM,
            CStdLib.IPPROTO_TCP
        );
    }

    /**
     * Close posix socket
     * @param socket Socket to close
     * @return Zero on success, -1 on error
     */
    private static int closePosixSocket(final int socket) {
        return CStdLib.INSTANCE.close(socket);
    }

    /**
     * Bind socket.
     * @param socket Socket
     * @return Zero on success, -1 on error
     */
    private static int bindPosixSocket(final int socket) {
        return EOsocketTest.bindPosixSocket(socket, 8080, EOsocketTest.LOCALHOST);
    }

    /**
     * Bind socket.
     * @param socket Socket
     * @param port Port
     * @param address Address
     * @return Zero on success, -1 on error
     */
    private static int bindPosixSocket(final int socket, final int port, final String address) {
        return CStdLib.INSTANCE.bind(
            socket,
            new SockaddrIn(
                (short) CStdLib.AF_INET,
                EOsocketTest.htons(port),
                EOsocketTest.posixInetAddr(address)
            ),
            16
        );
    }

    /**
     * Get last posix error.
     * @param error Error code
     * @return Last posix error as string
     */
    private static String getPosixError(final int error) {
        return CStdLib.INSTANCE.strerror(error);
    }

    /**
     * Get last posix error.
     * @return Last posix error as string
     */
    private static String getPosixError() {
        return EOsocketTest.getPosixError(Native.getLastError());
    }

    /**
     * Call posix inet addr.
     * @param address IP address
     * @return Posix inet addr as integer
     */
    private static int posixInetAddr(final String address) {
        return CStdLib.INSTANCE.inet_addr(address);
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
     * Start server on 8080 port.
     * @return Server socket
     * @throws IOException If fails to start
     */
    private static ServerSocket startServer() throws IOException {
        return EOsocketTest.startServer(8080);
    }

    /**
     * Start server on specific port.
     * @param port Port to start server on
     * @return Server socket
     * @throws IOException If fails to start
     */
    private static ServerSocket startServer(final int port) throws IOException {
        return new ServerSocket(port);
    }

    /**
     * Stop server.
     * @param socket Socket
     * @throws IOException If fails to stop
     */
    private static void stopServer(final ServerSocket socket) throws IOException {
        if (socket != null && !socket.isClosed()) {
            socket.close();
        }
    }

    /**
     * Scoped object.
     * @since 0.40.0
     */
    private static class Scoped extends PhDefault implements Atom {
        /**
         * Ctor.
         */
        @SuppressWarnings("PMD.ConstructorOnlyInitializesOrCallOtherConstructors")
        Scoped() {
            this.add("s", new AtVoid("s"));
        }

        @Override
        public Phi lambda() {
            return new Data.ToPhi(true);
        }
    }
}
