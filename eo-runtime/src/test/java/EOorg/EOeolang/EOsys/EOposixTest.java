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

import com.sun.jna.Native;
import EOorg.EOeolang.EOsys.Posix.CStdLib;
import EOorg.EOeolang.EOtuple$EOempty;
import java.io.IOException;
import java.lang.management.ManagementFactory;
import java.net.ServerSocket;
import java.nio.charset.StandardCharsets;
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
 * Test case for {@link EOposix}.
 * @checkstyle TypeNameCheck (100 lines)
 * @since 0.40
 */
@SuppressWarnings("JTCOP.RuleAllTestsHaveProductionClass")
final class EOposixTest {
    @Test
    @DisabledOnOs(OS.WINDOWS)
    void invokesGetpidCorrectly() {
        MatcherAssert.assertThat(
            "The \"getpid\" system call was expected to work correctly",
            new Dataized(
                new PhWith(
                    new PhWith(
                        Phi.Φ.take("org.eolang.sys.posix").copy(),
                        "name",
                        new Data.ToPhi("getpid")
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

    @Nested
    @DisabledOnOs(OS.WINDOWS)
    @Execution(ExecutionMode.SAME_THREAD)
    final class SocketSyscallsTest {
        @Test
        void connectsToLocalServerViaSyscall() throws IOException {
            final ServerSocket server = this.startServer();
            final int socket = CStdLib.INSTANCE.socket(
                CStdLib.AF_INET,
                CStdLib.SOCK_STREAM,
                CStdLib.IPPROTO_TCP
            );
            assert socket >= 0;
            final SockaddrIn addr = new SockaddrIn(
                (short) CStdLib.AF_INET,
                this.htons(8080),
                CStdLib.INSTANCE.inet_addr("127.0.0.1")
            );
            final int connected = CStdLib.INSTANCE.connect(socket, addr, addr.size());
            final String error;
            if (connected != 0) {
                error = CStdLib.INSTANCE.strerror(Native.getLastError());
            } else {
                error = "";
            }
            MatcherAssert.assertThat(
                String.format(
                    "Posix socket should have been connected to local server via syscall, but it didn't, error is: %s",
                    error
                ),
                connected,
                Matchers.equalTo(0)
            );
            assert CStdLib.INSTANCE.close(socket) == 0;
            this.stopServer(server);
        }

        @Test
        void refusesConnection() {
            final int socket = CStdLib.INSTANCE.socket(
                CStdLib.AF_INET,
                CStdLib.SOCK_STREAM,
                CStdLib.IPPROTO_TCP
            );
            assert socket >= 0;
            final SockaddrIn addr = new SockaddrIn(
                (short) CStdLib.AF_INET,
                this.htons(1234),
                CStdLib.INSTANCE.inet_addr("127.0.0.1")
            );
            final int connected = CStdLib.INSTANCE.connect(socket, addr, addr.size());
            MatcherAssert.assertThat(
                "Connection via syscall to wrong port must be refused",
                connected,
                Matchers.equalTo(-1)
            );
            assert CStdLib.INSTANCE.close(socket) == 0;
        }

        @Test
        void connectsToLocalServerViaSocketObject() throws IOException {
            final ServerSocket server = this.startServer();
            final Phi socket = Phi.Φ.take("org.eolang.net.socket")
                .take("posix-socket")
                .copy();
            socket.put(0, new Data.ToPhi("127.0.0.1"));
            socket.put(1, new Data.ToPhi(8080));
            final Phi connected = socket.take("connect").copy();
            connected.put(0, new Scoped());
            final byte[] expected = new byte[] {1};
            final byte[] actual = new Dataized(connected).take();
            MatcherAssert.assertThat(
                String.format(
                    "Posix socket.connect should have been successfully connected to local server, but it didn't, reason: %s",
                    new String(actual, StandardCharsets.UTF_8)
                ),
                actual,
                Matchers.equalTo(expected)
            );
            this.stopServer(server);
        }

        /**
         * Convert port number from host to network byte order (htons)
         * @param port Port number
         * @return Port number in network byte order
         */
        public short htons(final int port) {
            return (short) (((port & 0xFF) << 8) | ((port >> 8) & 0xFF));
        }

        /**
         * Start server on 8080 port.
         * @return Server socket
         * @throws IOException If fails to start
         */
        private ServerSocket startServer() throws IOException {
            return this.startServer(8080);
        }

        /**
         * Start server on specific port.
         * @param port Port to start server on
         * @return Server socket
         * @throws IOException If fails to start
         */
        private ServerSocket startServer(final int port) throws IOException {
            return new ServerSocket(port);
        }

        /**
         * Stop server.
         * @param socket Socket
         * @throws IOException If fails to stop
         */
        private void stopServer(final ServerSocket socket) throws IOException {
            if (socket != null && !socket.isClosed()) {
                socket.close();
            }
        }

        /**
         * Scoped object.
         * @since 0.40.0
         */
        private class Scoped extends PhDefault implements Atom {
            /**
             * Ctor.
             */
            Scoped() {
                this.add("s", new AtVoid("s"));
            }

            @Override
            public Phi lambda() {
                return new Data.ToPhi(true);
            }
        }
    }
}
