/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.posix;

import org.eolang.Cstring;
import org.eolang.Data;
import org.eolang.Int;
import org.eolang.PhDefault;
import org.eolang.Phi;
import org.eolang.Syscall;

/**
 * Access syscall.
 *
 * @since 0.74.0
 */
public final class AccessSyscall implements Syscall {

    /**
     * Posix object.
     */
    private final Phi posix;

    /**
     * Ctor.
     *
     * @param posix Posix object
     */
    public AccessSyscall(final Phi posix) {
        this.posix = posix;
    }

    @Override
    public Phi make(final Phi... params) {
        final String path = new Cstring("the 'path' argument of access", params[0]).it();
        final Phi result = this.posix.take("return").copy();
        result.put(
            0,
            new Data.ToPhi(
                CStdLib.INSTANCE.access(
                    path,
                    new Int("the 'mode' argument of access", params[1]).it()
                )
            )
        );
        result.put(1, new PhDefault());
        return result;
    }
}
