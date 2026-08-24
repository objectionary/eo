/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.posix;

import com.sun.jna.Native;
import org.eolang.Data;
import org.eolang.Phi;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
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
    void leavesErrnoAloneOnAValidAddress() {
        Native.setLastError(0);
        new InetAddrSyscall(Phi.Φ.take("posix").copy()).make(new Data.ToPhi("127.0.0.1"));
        MatcherAssert.assertThat(
            "A successful inet_addr conversion must not fabricate an error",
            Native.getLastError(),
            Matchers.equalTo(0)
        );
    }
}
