/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.posix;

import com.sun.jna.ptr.IntByReference;
import org.eolang.Data;
import org.eolang.Int;
import org.eolang.PhDefault;
import org.eolang.Phi;
import org.eolang.Sockaddr;
import org.eolang.Syscall;

/**
 * Accept syscall.
 * @since 0.40
 */
public final class AcceptSyscall implements Syscall {

    /**
     * Posix object.
     */
    private final Phi posix;

    /**
     * Ctor.
     * @param posix Posix object
     */
    public AcceptSyscall(final Phi posix) {
        this.posix = posix;
    }

    @Override
    public Phi make(final Phi... params) {
        final Phi result = this.posix.take("return").copy();
        result.put(
            0,
            new Data.ToPhi(
                CStdLib.INSTANCE.accept(
                    new Int("the 'descriptor' argument of accept", params[0]).it(),
                    new Sockaddr(params[1]).it(),
                    new IntByReference(new Int("the 'length' argument of accept", params[2]).it())
                )
            )
        );
        result.put(1, new PhDefault());
        return result;
    }
}
