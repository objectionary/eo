/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.posix;

import org.eolang.Data;
import org.eolang.Int;
import org.eolang.PhDefault;
import org.eolang.Phi;
import org.eolang.Syscall;

/**
 * Socket syscall.
 *
 * @since 0.40
 */
public final class SocketSyscall implements Syscall {

    /**
     * Posix object.
     */
    private final Phi posix;

    /**
     * Ctor.
     *
     * @param posix Posix object
     */
    public SocketSyscall(final Phi posix) {
        this.posix = posix;
    }

    @Override
    public Phi make(final Phi... params) {
        final Phi result = this.posix.take("return").copy();
        result.put(
            0,
            new Data.ToPhi(
                CStdLib.INSTANCE.socket(
                    new Int("the 'domain' argument of socket", params[0]).it(),
                    new Int("the 'type' argument of socket", params[1]).it(),
                    new Int("the 'protocol' argument of socket", params[2]).it()
                )
            )
        );
        result.put(1, new PhDefault());
        return result;
    }
}
