/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.sys.win32;

import com.sun.jna.Pointer;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.DisabledOnOs;
import org.junit.jupiter.api.condition.OS;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;

/**
 * Test case for {@link Winsock}.
 *
 * @since 0.40
 */
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
