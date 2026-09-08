/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import com.sun.jna.Pointer;
import java.io.IOException;
import java.lang.management.ManagementFactory;
import java.nio.file.Files;
import java.nio.file.Path;
import org.eolang.win32.WSAData;
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
 *
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
    void accessesFileWithNonAsciiName() throws IOException {
        final Path file = Files.createTempFile("Ж日本-", ".txt");
        MatcherAssert.assertThat(
            String.format("\"_access\" did not find the non-ASCII file %s", file),
            new Dataized(
                new PhApplication(
                    new PhApplication(
                        Phi.Φ.take("win32").copy(),
                        "name",
                        new Data.ToPhi("_access")
                    ),
                    "args",
                    new Data.ToPhi(
                        new Phi[]{
                            new Data.ToPhi(file.toString()),
                            new Data.ToPhi(0),
                        }
                    )
                ).take("code")
            ).asNumber().intValue(),
            Matchers.equalTo(0)
        );
    }

    /**
     * Test case for {@link Winsock}.
     *
     * @since 0.40
     */
    @Nested
    @Execution(ExecutionMode.SAME_THREAD)
    @DisabledOnOs({OS.LINUX, OS.MAC})
    final class WinsockTest {

        @Test
        void initializesWinsockLibrary() {
            MatcherAssert.assertThat(
                "Winsock library should be successfully initialized, but it isn't",
                this.startupsWinsock(),
                Matchers.equalTo(0)
            );
            this.cleanupsWinsock();
        }

        @Test
        void cleansupWinsockLibrary() {
            this.startupsWinsock();
            MatcherAssert.assertThat(
                "Winsock library resources should be freed successfully",
                this.cleanupsWinsock(),
                Matchers.equalTo(0)
            );
        }

        @Test
        void opensTcpSocket() {
            this.startupsWinsock();
            final long socket = this.createsSocket();
            MatcherAssert.assertThat(
                "Winsock library should successfully create a TCP socket, but it didn't",
                socket,
                Matchers.not(Matchers.equalTo(Winsock.INVALID_SOCKET))
            );
            this.closesSocket(socket);
            this.cleanupsWinsock();
        }

        @Test
        void closesTcpSocket() {
            this.startupsWinsock();
            MatcherAssert.assertThat(
                "Winsock library should successfully close a TCP socket, but it didn't",
                this.closesSocket(this.createsSocket()),
                Matchers.not(Matchers.equalTo(Winsock.SOCKET_ERROR))
            );
            this.cleanupsWinsock();
        }

        private long createsSocket() {
            return Pointer.nativeValue(
                Winsock.INSTANCE.socket(
                    Winsock.AF_INET,
                    Winsock.SOCK_STREAM,
                    Winsock.IPPROTO_TCP
                )
            );
        }

        private int closesSocket(final long socket) {
            return Winsock.INSTANCE.closesocket(new Pointer(socket));
        }

        private int startupsWinsock() {
            return Winsock.INSTANCE.WSAStartup(
                Winsock.VERSION_2_2,
                new WSAData()
            );
        }

        private int cleanupsWinsock() {
            return Winsock.INSTANCE.WSACleanup();
        }
    }
}
