/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.posix;

import org.eolang.Data;
import org.eolang.Int;
import org.eolang.PhDefault;
import org.eolang.Phi;
import org.eolang.Sockaddr;
import org.eolang.Syscall;

/**
 * Bind syscall.
 * @since 0.40
 */
public final class BindSyscall implements Syscall {

    /**
     * Posix object.
     */
    private final Phi posix;

    /**
     * Ctor.
     * @param posix Posix object
     */
    public BindSyscall(final Phi posix) {
        this.posix = posix;
    }

    @Override
    public Phi make(final Phi... params) {
        final Phi result = this.posix.take("return").copy();
        result.put(
            0,
            new Data.ToPhi(
                CStdLib.INSTANCE.bind(
                    new Int("the 'descriptor' argument of bind", params[0]).it(),
                    new Sockaddr(params[1]).it(),
                    new Int("the 'length' argument of bind", params[2]).it()
                )
            )
        );
        result.put(1, new PhDefault());
        return result;
    }
}
