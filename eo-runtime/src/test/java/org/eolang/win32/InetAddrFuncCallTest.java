/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.win32;

import java.util.Collections;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.Phi;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.DisabledOnOs;
import org.junit.jupiter.api.condition.OS;

/**
 * Test case for {@link InetAddrFuncCall}.
 * @since 0.75.0
 */
final class InetAddrFuncCallTest {

    @Test
    @DisabledOnOs({OS.MAC, OS.LINUX})
    void readsALeadingZeroAsOctal() {
        MatcherAssert.assertThat(
            "a part behind a leading zero is octal to inet_addr, so 010 is 8, and reading it as decimal ten sends an EO program to a different host than the POSIX half of this call sends it to",
            InetAddrFuncCallTest.converted("010.1.1.1"),
            Matchers.equalTo(16_843_016.0d)
        );
    }

    @Test
    @DisabledOnOs({OS.MAC, OS.LINUX})
    void acceptsTheTwoPartForm() {
        MatcherAssert.assertThat(
            "the last part of a shortened address fills the bytes still left, so 127.1 is 127.0.0.1, not text to refuse",
            InetAddrFuncCallTest.converted("127.1"),
            Matchers.equalTo(16_777_343.0d)
        );
    }

    @Test
    @DisabledOnOs({OS.MAC, OS.LINUX})
    void widensTheAddressToUnsigned() {
        MatcherAssert.assertThat(
            "inet_addr answers with an in_addr_t, which is unsigned, so an address whose last byte is 128 or above must not reach EO as a negative number",
            InetAddrFuncCallTest.converted("10.0.0.200"),
            Matchers.equalTo(3_355_443_210.0d)
        );
    }

    @Test
    @DisabledOnOs({OS.MAC, OS.LINUX})
    void doesNotReportInvalidForTheBroadcastAddress() {
        Winsock.INSTANCE.WSASetLastError(0);
        InetAddrFuncCallTest.converted(String.join(".", Collections.nCopies(4, "255")));
        MatcherAssert.assertThat(
            "the limited-broadcast address converts to INADDR_NONE like unconvertable text does, but it is valid, so WSAEINVAL must not be reported for it",
            Winsock.INSTANCE.WSAGetLastError(),
            Matchers.not(Matchers.equalTo(Winsock.WSAEINVAL))
        );
    }

    @Test
    @DisabledOnOs({OS.MAC, OS.LINUX})
    void reportsInvalidOnUnconvertableText() {
        InetAddrFuncCallTest.converted("nope");
        MatcherAssert.assertThat(
            "inet_addr does not set the last winsock error itself, so the call must set it, or a later WSAGetLastError reads whatever an unrelated earlier call left behind",
            Winsock.INSTANCE.WSAGetLastError(),
            Matchers.equalTo(Winsock.WSAEINVAL)
        );
    }

    private static Double converted(final String address) {
        return new Dataized(
            new InetAddrFuncCall(Phi.Φ.take("win32").copy())
                .make(new Data.ToPhi(address))
                .take("code")
        ).asNumber();
    }
}
