/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import java.lang.management.ManagementFactory;
import org.eolang.win32.WSAStartupFuncCall;
import org.eolang.win32.Winsock;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.DisabledOnOs;
import org.junit.jupiter.api.condition.OS;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;

/**
 * Test case for {@code EOwin32$EOφ}.
 * @since 0.40
 */
final class EOwin32EOφTest {

    @Test
    @DisabledOnOs({OS.LINUX, OS.MAC})
    void invokesGetpidCorrectly() {
        MatcherAssert.assertThat(
            "The \"_getpid\" function call was expected to work correctly",
            new Dataized(
                new PhApplication(
                    new PhApplication(
                        Phi.Φ.take("win32").copy(),
                        "name",
                        new Data.ToPhi("_getpid")
                    ),
                    "args",
                    Phi.Φ.take("tuple").take("empty")
                ).take("code")
            ).asNumber().intValue(),
            Matchers.equalTo(
                Integer.parseInt(
                    ManagementFactory.getRuntimeMXBean()
                        .getName().split("@", -1)[0]
                )
            )
        );
    }

    @Test
    @DisabledOnOs({OS.LINUX, OS.MAC})
    void reportsReasonWhenOpenFails() {
        MatcherAssert.assertThat(
            "Failed \"_open\" should carry the OS error reason in its output",
            new Dataized(
                new PhApplication(
                    new PhApplication(
                        Phi.Φ.take("win32").copy(),
                        "name",
                        new Data.ToPhi("_open")
                    ),
                    "args",
                    new Data.ToPhi(
                        new Phi[]{
                            new Data.ToPhi("C:\\eo-5403-absent-directory\\file.txt"),
                            new Data.ToPhi(0),
                            new Data.ToPhi(0),
                        }
                    )
                ).take("output")
            ).asString(),
            Matchers.containsString("No such file")
        );
    }

    @Test
    @DisabledOnOs({OS.LINUX, OS.MAC})
    void acceptsDottedIpv4Literal() {
        MatcherAssert.assertThat(
            "The \"inet_addr\" function call should have parsed a dotted IPv4 literal",
            this.inetAddr("1.2.3.4"),
            Matchers.equalTo(67_305_985)
        );
    }

    @Test
    @DisabledOnOs({OS.LINUX, OS.MAC})
    void rejectsHostName() {
        MatcherAssert.assertThat(
            "The \"inet_addr\" function call should have rejected a host name, not resolved it",
            this.inetAddr("localhost"),
            Matchers.equalTo(-1)
        );
    }

    @Test
    @DisabledOnOs({OS.LINUX, OS.MAC})
    void rejectsIpv6Literal() {
        MatcherAssert.assertThat(
            "The \"inet_addr\" function call should have rejected an IPv6 literal",
            this.inetAddr("::1"),
            Matchers.equalTo(-1)
        );
    }

    /**
     * Calls "inet_addr" on the win32 object with the given address.
     * @param address Address to resolve
     * @return Resolved code
     */
    private int inetAddr(final String address) {
        return new Dataized(
            new PhApplication(
                new PhApplication(
                    Phi.Φ.take("win32").copy(),
                    "name",
                    new Data.ToPhi("inet_addr")
                ),
                "args",
                new Data.ToPhi(new Phi[]{new Data.ToPhi(address)})
            ).take("code")
        ).asNumber().intValue();
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
