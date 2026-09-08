/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.posix;

import org.eolang.Cstring;
import org.eolang.Data;
import org.eolang.Phi;
import org.eolang.Syscall;

/**
 * Rename syscall.
 *
 * @since 0.74.0
 */
public final class RenameSyscall implements Syscall {

    /**
     * Posix object.
     */
    private final Phi posix;

    /**
     * Ctor.
     *
     * @param posix Posix object
     */
    public RenameSyscall(final Phi posix) {
        this.posix = posix;
    }

    @Override
    public Phi make(final Phi... params) {
        final String from = new Cstring("the 'from' argument of rename", params[0]).it();
        final String target = new Cstring("the 'to' argument of rename", params[1]).it();
        final Phi result = this.posix.take("return").copy();
        final int code = CStdLib.INSTANCE.rename(from, target);
        result.put(0, new Data.ToPhi(code));
        result.put(1, new Errno(code).get());
        return result;
    }
}
