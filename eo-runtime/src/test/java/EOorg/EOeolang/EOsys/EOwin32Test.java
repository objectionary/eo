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
package EOorg.EOeolang.EOsys; // NOPMD

import EOorg.EOeolang.EOsys.Win32.WSAStartupFuncCall;
import EOorg.EOeolang.EOsys.Win32.Winsock;
import EOorg.EOeolang.EOtuple$EOempty;
import com.jcabi.log.Logger;
import java.io.IOException;
import java.lang.management.ManagementFactory;
import java.net.ServerSocket;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import org.eolang.AtVoid;
import org.eolang.Atom;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.PhDefault;
import org.eolang.PhWith;
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
 * Test case for {@link EOwin32}.
 *
 * @since 0.40
 * @checkstyle TypeNameCheck (100 lines)
 */
@SuppressWarnings("JTCOP.RuleAllTestsHaveProductionClass")
@Execution(ExecutionMode.SAME_THREAD)
final class EOwin32Test {
    @Test
    @DisabledOnOs({OS.LINUX, OS.MAC, OS.AIX})
    void invokesGetCurrentProcessIdCorrectly() {
        MatcherAssert.assertThat(
            "The \"GetCurrentProcessId\" function call was expected to work correctly",
            new Dataized(
                new PhWith(
                    new PhWith(
                        Phi.Φ.take("org.eolang.sys.win32").copy(),
                        "name",
                        new Data.ToPhi("GetCurrentProcessId")
                    ),
                    "args",
                    new EOtuple$EOempty()
                ).take("code")
            ).asNumber().intValue(),
            Matchers.equalTo(
                Integer.parseInt(
                    ManagementFactory.getRuntimeMXBean()
                        .getName().split("@")[0]
                )
            )
        );
    }

    /**
     * Creates socket.
     * @return Closes socket
     */
    private static int createsSocket() {
        return Winsock.INSTANCE.socket(
            Winsock.AF_INET,
            Winsock.SOCK_STREAM,
            Winsock.IPPROTO_TCP
        );
    }

    /**
     * Closes socket.
     * @param socket Socket descriptor
     * @return Status code
     */
    private static int closesSocket(final int socket) {
        return Winsock.INSTANCE.closesocket(socket);
    }

    /**
     * Startups winsock library.
     * @return Status code
     * @checkstyle AbbreviationAsWordInNameCheck (5 lines)
     */
    private static int startupsWSA() {
        return Winsock.INSTANCE.WSAStartup(
            Winsock.WINSOCK_VERSION_2_2,
            new WSAStartupFuncCall.WSAData()
        );
    }

    /**
     * Cleans up winsock library.
     * @return Status code
     * @checkstyle AbbreviationAsWordInNameCheck (5 lines)
     */
    private static int cleanupsWSA() {
        return Winsock.INSTANCE.WSACleanup();
    }

    /**
     * Helper function to convert port number to network byte order (htons).
     * @param port Port
     * @return Port in network byte order
     */
    private static short htons(final int port) {
        return (short) (((port & 0xFF) << 8) | ((port >> 8) & 0xFF));
    }

    /**
     * Start local server socket.
     * @return Local server socket
     * @throws IOException If fails
     */
    private static ServerSocket startServer() throws IOException {
        return new ServerSocket(8080);
    }

    /**
     * Stop local server.
     * @param socket Local server socket
     * @throws IOException If fails
     */
    private static void stopServer(final ServerSocket socket) throws IOException {
        if (socket != null && !socket.isClosed()) {
            socket.close();
        }
    }

    /**
     * Test case for {@link Winsock}.
     * @since 0.40
     * @checkstyle AbbreviationAsWordInNameCheck (300 lines)
     */
    @Nested
    @Execution(ExecutionMode.SAME_THREAD)
    @DisabledOnOs({OS.LINUX, OS.MAC, OS.AIX})
    final class WinsockTest {
        @Test
        void initializesWinsockLibrary() {
            MatcherAssert.assertThat(
                "Winsock library should be successfully initialized, but it isn't",
                EOwin32Test.startupsWSA(),
                Matchers.equalTo(0)
            );
            EOwin32Test.cleanupsWSA();
        }

        @Test
        void cleansupWinsockLibrary() {
            EOwin32Test.startupsWSA();
            MatcherAssert.assertThat(
                "Winsock library resources should be freed successfully",
                EOwin32Test.cleanupsWSA(),
                Matchers.equalTo(0)
            );
        }

        @Test
        void opensTcpSocket() {
            EOwin32Test.startupsWSA();
            final int socket = EOwin32Test.createsSocket();
            MatcherAssert.assertThat(
                "Winsock library should successfully create a TCP socket, but it didn't",
                socket,
                Matchers.not(Matchers.equalTo(Winsock.INVALID_SOCKET))
            );
            EOwin32Test.closesSocket(socket);
            EOwin32Test.cleanupsWSA();
        }

        @Test
        void closesTcpSocket() {
            EOwin32Test.startupsWSA();
            final int socket = EOwin32Test.createsSocket();
            MatcherAssert.assertThat(
                "Winsock library should successfully close a TCP socket, but it didn't",
                EOwin32Test.closesSocket(socket),
                Matchers.not(Matchers.equalTo(Winsock.SOCKET_ERROR))
            );
            EOwin32Test.cleanupsWSA();
        }

        @Test
        void connectsSuccessfullyViaSyscalls() throws IOException {
            final ServerSocket socket = EOwin32Test.startServer();
            assert EOwin32Test.startupsWSA() == 0;
            final int descriptor = EOwin32Test.createsSocket();
            assert descriptor >= 0;
            final int connected = Winsock.INSTANCE.connect(
                descriptor,
                new SockaddrIn(
                    (short) Winsock.AF_INET,
                    EOwin32Test.htons(8080),
                    Winsock.INSTANCE.inet_addr("127.0.0.1")
                ),
                16
            );
            if (connected != 0) {
                Logger.info(this, "Error code: %d", Winsock.INSTANCE.WSAGetLastError());
            }
            MatcherAssert.assertThat(
                "Winsock 'connect' func call should have successfully connected to local server, but it didn't",
                connected,
                Matchers.equalTo(0)
            );
            assert EOwin32Test.closesSocket(descriptor) == 0;
            EOwin32Test.cleanupsWSA();
            EOwin32Test.stopServer(socket);
        }

        @Test
        void connectsViaSocketObject() throws IOException {
            final ServerSocket socket = EOwin32Test.startServer();
            final Phi sock = Phi.Φ.take("org.eolang.net.socket").take("win-socket").copy();
            sock.put(0, new Data.ToPhi("127.0.0.1"));
            sock.put(1, new Data.ToPhi(8080));
            final Phi connected = sock.take("connect").copy();
            connected.put(0, new EOwin32Test.Scope());
            final byte[] result = new Dataized(connected).take();
            final byte[] expected = new byte[] {1};
            if (!Arrays.equals(result, expected)) {
                Logger.info(this, "Error code: %d", Winsock.INSTANCE.WSAGetLastError());
            }
            MatcherAssert.assertThat(
                String.format(
                    "Windows socket should have connected successfully to local server, but it didn't, message is: %s",
                    new String(result, StandardCharsets.UTF_8)
                ),
                result, Matchers.equalTo(expected)
            );
            EOwin32Test.stopServer(socket);
        }
    }

    /**
     * Socket scope.
     * @since 0.40.0
     */
    private static final class Scope extends PhDefault implements Atom {
        /**
         * Ctor.
         */
        @SuppressWarnings("PMD.ConstructorOnlyInitializesOrCallOtherConstructors")
        Scope() {
            this.add("s", new AtVoid("s"));
        }

        @Override
        public Phi lambda() {
            return new Data.ToPhi(true);
        }
    }
}
