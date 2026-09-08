/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.win32;

import org.eolang.Data;
import org.eolang.Int;
import org.eolang.PhDefault;
import org.eolang.Phi;
import org.eolang.Syscall;

/**
 * The msvcrt _close function call.
 *
 * @since 0.74.0
 */
public final class CloseFuncCall implements Syscall {

    /**
     * Win32 object.
     */
    private final Phi win;

    /**
     * Ctor.
     *
     * @param win Win32 object
     */
    public CloseFuncCall(final Phi win) {
        this.win = win;
    }

    @Override
    public Phi make(final Phi... params) {
        final int descriptor = new Int(
            "the 'descriptor' argument of close", params[0]
        ).it();
        final Phi result = this.win.take("return").copy();
        result.put(
            0,
            new Data.ToPhi(
                Msvcrt.INSTANCE._close(descriptor)
            )
        );
        result.put(1, new PhDefault());
        return result;
    }
}
