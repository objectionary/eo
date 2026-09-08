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
 * Creat syscall.
 *
 * @since 0.74.0
 */
public final class CreatSyscall implements Syscall {

    /**
     * Posix object.
     */
    private final Phi posix;

    /**
     * Ctor.
     *
     * @param posix Posix object
     */
    public CreatSyscall(final Phi posix) {
        this.posix = posix;
    }

    @Override
    public Phi make(final Phi... params) {
        final String path = new Cstring("the 'path' argument of creat", params[0]).it();
        final Phi result = this.posix.take("return").copy();
        final int code = CStdLib.INSTANCE.creat(
            path,
            new Int("the 'mode' argument of creat", params[1]).it()
        );
        result.put(0, new Data.ToPhi(code));
        result.put(1, new Errno(code).get());
        return result;
    }
}
