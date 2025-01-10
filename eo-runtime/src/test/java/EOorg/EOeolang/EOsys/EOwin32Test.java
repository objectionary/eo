/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2025 Objectionary.com
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
import java.lang.management.ManagementFactory;
import org.eolang.Data;
import org.eolang.Dataized;
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
final class EOwin32Test {
    @Test
    @DisabledOnOs({OS.LINUX, OS.MAC})
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
     * Test case for {@link Winsock}.
     * @since 0.40
     * @checkstyle AbbreviationAsWordInNameCheck (300 lines)
     */
    @Nested
    @Execution(ExecutionMode.SAME_THREAD)
    @DisabledOnOs({OS.LINUX, OS.MAC})
    final class WinsockTest {
        @Test
        void initializesWinsockLibrary() {
            MatcherAssert.assertThat(
                "Winsock library should be successfully initialized, but it isn't",
                this.startupsWSA(),
                Matchers.equalTo(0)
            );
            this.cleanupsWSA();
        }

        @Test
        void cleansupWinsockLibrary() {
            this.startupsWSA();
            MatcherAssert.assertThat(
                "Winsock library resources should be freed successfully",
                this.cleanupsWSA(),
                Matchers.equalTo(0)
            );
        }

        @Test
        void opensTcpSocket() {
            this.startupsWSA();
            final int socket = this.createsSocket();
            MatcherAssert.assertThat(
                "Winsock library should successfully create a TCP socket, but it didn't",
                socket,
                Matchers.not(Matchers.equalTo(Winsock.INVALID_SOCKET))
            );
            this.closesSocket(socket);
            this.cleanupsWSA();
        }

        @Test
        void closesTcpSocket() {
            this.startupsWSA();
            MatcherAssert.assertThat(
                "Winsock library should successfully close a TCP socket, but it didn't",
                this.closesSocket(this.createsSocket()),
                Matchers.not(Matchers.equalTo(Winsock.SOCKET_ERROR))
            );
            this.cleanupsWSA();
        }

        /**
         * Creates socket.
         * @return Closes socket
         */
        private int createsSocket() {
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
        private int closesSocket(final int socket) {
            return Winsock.INSTANCE.closesocket(socket);
        }

        /**
         * Startups winsock library.
         * @return Status code
         */
        private int startupsWSA() {
            return Winsock.INSTANCE.WSAStartup(
                Winsock.WINSOCK_VERSION_2_2,
                new WSAStartupFuncCall.WSAData()
            );
        }

        /**
         * Cleans up winsock library.
         * @return Status code
         */
        private int cleanupsWSA() {
            return Winsock.INSTANCE.WSACleanup();
        }
    }
}
