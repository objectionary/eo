/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.win32;

import com.sun.jna.Native;
import java.util.Collections;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.ExFailure;
import org.eolang.Phi;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.DisabledOnOs;
import org.junit.jupiter.api.condition.OS;

/**
 * Test case for {@link InetAddrFuncCall}.
 *
 * <p>The tests that watch the last winsock error read it from
 * {@link Native#getLastError()}, straight after the call and before anything
 * is dataized: that read hands over the copy JNA takes of the per-thread
 * last-error slot the instant every mapped call returns, and any later call
 * of ours would replace the copy with its own.</p>
 *
 * @since 0.75.0
 */
final class InetAddrFuncCallTest {

    @Test
    @DisabledOnOs({OS.MAC, OS.LINUX})
    void readsALeadingZeroAsOctal() {
        MatcherAssert.assertThat(
            "a part behind a leading zero is octal to inet_addr, so 010 is 8, and reading it as decimal ten sends an EO program to a different host than the POSIX half of this call sends it to",
            this.converted("010.1.1.1"),
            Matchers.equalTo(16_843_016.0d)
        );
    }

    @Test
    @DisabledOnOs({OS.MAC, OS.LINUX})
    void acceptsTheTwoPartForm() {
        MatcherAssert.assertThat(
            "the last part of a shortened address fills the bytes still left, so 127.1 is 127.0.0.1, not text to refuse",
            this.converted("127.1"),
            Matchers.equalTo(16_777_343.0d)
        );
    }

    @Test
    @DisabledOnOs({OS.MAC, OS.LINUX})
    void widensTheAddressToUnsigned() {
        MatcherAssert.assertThat(
            "inet_addr answers with an in_addr_t, which is unsigned, so an address whose last byte is 128 or above must not reach EO as a negative number",
            this.converted("10.0.0.200"),
            Matchers.equalTo(3_355_443_210.0d)
        );
    }

    @Test
    @DisabledOnOs({OS.MAC, OS.LINUX})
    void doesNotReportInvalidForTheBroadcastAddress() {
        Native.setLastError(0);
        this.made(String.join(".", Collections.nCopies(4, "255")));
        MatcherAssert.assertThat(
            "the limited-broadcast address converts to INADDR_NONE like unconvertible text does, but it is valid, so WSAEINVAL must not be reported for it",
            Native.getLastError(),
            Matchers.not(Matchers.equalTo(Winsock.WSAEINVAL))
        );
    }

    @Test
    @DisabledOnOs({OS.MAC, OS.LINUX})
    void reportsInvalidOnUnconvertibleText() {
        Native.setLastError(0);
        this.made("nope");
        MatcherAssert.assertThat(
            "inet_addr does not set the last winsock error itself, so the call must set it, or a later read of the last error answers with whatever an unrelated earlier call left behind",
            Native.getLastError(),
            Matchers.equalTo(Winsock.WSAEINVAL)
        );
    }

    @Test
    void refusesAnAddressWithNul() {
        MatcherAssert.assertThat(
            "the 'address' argument of inet_addr carrying a NUL must be refused by name, but it wasnt",
            Assertions.assertThrows(
                ExFailure.class,
                () -> new InetAddrFuncCall(Phi.Φ.take("win32").copy()).make(
                    new Data.ToPhi(String.join(String.valueOf((char) 0), "127.0.0.1", "suffix"))
                ),
                "an address whose NUL would make inet_addr convert only its prefix was expected to fail with ExFailure"
            ).getMessage(),
            Matchers.allOf(
                Matchers.containsString("'address' argument of inet_addr"),
                Matchers.containsString("NUL")
            )
        );
    }

    private Double converted(final String address) {
        return new Dataized(
            this.made(address).take("code")
        ).asNumber();
    }

    private Phi made(final String address) {
        return new InetAddrFuncCall(Phi.Φ.take("win32").copy())
            .make(new Data.ToPhi(address));
    }
}
