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
 * Open syscall.
 *
 * @since 0.74.0
 */
public final class OpenSyscall implements Syscall {

    /**
     * Posix object.
     */
    private final Phi posix;

    /**
     * Ctor.
     *
     * @param posix Posix object
     */
    public OpenSyscall(final Phi posix) {
        this.posix = posix;
    }

    @Override
    public Phi make(final Phi... params) {
        final String path = new Cstring("the 'path' argument of open", params[0]).it();
        final Phi result = this.posix.take("return").copy();
        final int code = CStdLib.INSTANCE.open(
            path,
            new Int("the 'flags' argument of open", params[1]).it(),
            new Int("the 'mode' argument of open", params[2]).it()
        );
        result.put(0, new Data.ToPhi(code));
        result.put(1, new Errno(code).get());
        return result;
    }
}
