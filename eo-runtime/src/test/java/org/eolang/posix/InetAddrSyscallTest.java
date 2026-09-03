/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.posix;

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
 * Test case for {@link InetAddrSyscall}.
 * @since 0.74.1
 */
final class InetAddrSyscallTest {

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void setsErrnoToInvalOnAnUnparsableAddress() {
        new InetAddrSyscall(Phi.Φ.take("posix").copy()).make(new Data.ToPhi("nope"));
        MatcherAssert.assertThat(
            "inet_addr does not set errno on failure, so the syscall wrapper must set it itself, to EINVAL, or a later strerror(errno) reads whatever an unrelated earlier call left behind",
            Native.getLastError(),
            Matchers.equalTo(22)
        );
    }

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void doesNotReportInvalidForTheBroadcastAddress() {
        Native.setLastError(0);
        new InetAddrSyscall(Phi.Φ.take("posix").copy()).make(
            new Data.ToPhi(String.join(".", Collections.nCopies(4, "255")))
        );
        MatcherAssert.assertThat(
            "the limited-broadcast address converts to INADDR_NONE like an unparsable one does, but it is valid, so EINVAL must not be reported for it",
            Native.getLastError(),
            Matchers.not(Matchers.equalTo(22))
        );
    }

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void widensTheAddressToUnsigned() {
        MatcherAssert.assertThat(
            "inet_addr returns in_addr_t, which POSIX defines as unsigned, so an address whose first octet is 128 or above must not reach EO as a negative number",
            new Dataized(
                new InetAddrSyscall(Phi.Φ.take("posix").copy())
                    .make(new Data.ToPhi("10.0.0.200"))
                    .take("code")
            ).asNumber(),
            Matchers.equalTo(3_355_443_210.0d)
        );
    }

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void leavesErrnoAloneOnAValidAddress() {
        Native.setLastError(0);
        new InetAddrSyscall(Phi.Φ.take("posix").copy()).make(new Data.ToPhi("127.0.0.1"));
        MatcherAssert.assertThat(
            "A successful inet_addr conversion must not fabricate an error",
            Native.getLastError(),
            Matchers.equalTo(0)
        );
    }

    @Test
    void refusesAnAddressWithNul() {
        MatcherAssert.assertThat(
            "the 'address' argument of inet_addr carrying a NUL must be refused by name, but it wasnt",
            Assertions.assertThrows(
                ExFailure.class,
                () -> new InetAddrSyscall(Phi.Φ.take("posix").copy()).make(
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
}
