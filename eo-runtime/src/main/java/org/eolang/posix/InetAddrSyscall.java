/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.posix;

import com.sun.jna.Native;
import java.util.Collections;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.PhDefault;
import org.eolang.Phi;
import org.eolang.Syscall;

/**
 * The 'inet_addr' syscall.
 *
 * <p>{@code inet_addr} returns {@code in_addr_t}, which POSIX defines as an
 * unsigned 32-bit integer, so the C {@code int} is widened before it becomes
 * an EO number: {@code 10.0.0.200} reads as 3355443210, not as -939524086,
 * and the {@code INADDR_NONE} the function reports for text it cannot parse
 * reads as 4294967295, the {@code posix.unresolved} constant.</p>
 *
 * @since 0.40
 */
public final class InetAddrSyscall implements Syscall {

    /**
     * The limited-broadcast address, whose conversion is {@code INADDR_NONE}
     * — the same value {@code inet_addr} returns for text it cannot parse,
     * which is why the two are told apart by the text rather than by the
     * result. {@code socket.eo} makes the same comparison for the same
     * reason.
     */
    private static final String BROADCAST = String.join(
        ".", Collections.nCopies(4, "255")
    );

    /**
     * Posix object.
     */
    private final Phi posix;

    /**
     * Ctor.
     * @param posix Posix object
     */
    public InetAddrSyscall(final Phi posix) {
        this.posix = posix;
    }

    @Override
    public Phi make(final Phi... params) {
        final Phi result = this.posix.take("return").copy();
        final String address = new Dataized(params[0]).asString();
        final int converted = CStdLib.INSTANCE.inet_addr(address);
        if (converted == -1 && !InetAddrSyscall.BROADCAST.equals(address)) {
            final int einval = 22;
            Native.setLastError(einval);
        }
        result.put(0, new Data.ToPhi(Integer.toUnsignedLong(converted)));
        result.put(1, new PhDefault());
        return result;
    }
}
