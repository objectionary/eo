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
 * Close syscall.
 *
 * @since 0.40
 */
public final class CloseSyscall implements Syscall {

    /**
     * Posix object.
     */
    private final Phi posix;

    /**
     * Ctor.
     *
     * @param posix Posix object
     */
    public CloseSyscall(final Phi posix) {
        this.posix = posix;
    }

    @Override
    public Phi make(final Phi... params) {
        final int descriptor = new Int(
            "the 'descriptor' argument of close", params[0]
        ).it();
        final Phi result = this.posix.take("return").copy();
        result.put(0, new Data.ToPhi(CStdLib.INSTANCE.close(descriptor)));
        result.put(1, new PhDefault());
        return result;
    }
}
