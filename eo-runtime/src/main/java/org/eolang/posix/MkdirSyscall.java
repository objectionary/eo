/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.posix;

import org.eolang.Cstring;
import org.eolang.Data;
import org.eolang.Int;
import org.eolang.Phi;
import org.eolang.Syscall;

/**
 * Mkdir syscall.
 *
 * @since 0.74.0
 */
public final class MkdirSyscall implements Syscall {

    /**
     * Posix object.
     */
    private final Phi posix;

    /**
     * Ctor.
     *
     * @param posix Posix object
     */
    public MkdirSyscall(final Phi posix) {
        this.posix = posix;
    }

    @Override
    public Phi make(final Phi... params) {
        final String path = new Cstring("the 'path' argument of mkdir", params[0]).it();
        final Phi result = this.posix.take("return").copy();
        final int code = CStdLib.INSTANCE.mkdir(
            path,
            new Int("the 'mode' argument of mkdir", params[1]).it()
        );
        result.put(0, new Data.ToPhi(code));
        result.put(1, new Errno(code).get());
        return result;
    }
}
