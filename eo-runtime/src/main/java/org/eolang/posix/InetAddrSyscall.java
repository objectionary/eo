/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.posix;

import com.sun.jna.Native;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.PhDefault;
import org.eolang.Phi;
import org.eolang.Syscall;

/**
 * The 'inet_addr' syscall.
 * @since 0.40
 */
public final class InetAddrSyscall implements Syscall {

    /**
     * {@code EINVAL}, the same value on every POSIX platform this library
     * targets (Linux, macOS).
     */
    private static final int EINVAL = 22;

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
        final int converted = CStdLib.INSTANCE.inet_addr(
            new Dataized(params[0]).asString()
        );
        if (converted == -1) {
            Native.setLastError(InetAddrSyscall.EINVAL);
        }
        result.put(0, new Data.ToPhi(converted));
        result.put(1, new PhDefault());
        return result;
    }
}
