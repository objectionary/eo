/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.win32;

import org.eolang.Data;
import org.eolang.PhDefault;
import org.eolang.Phi;
import org.eolang.Syscall;

/**
 * The msvcrt {@code _errno} function call: the raw CRT error code left by the
 * last failed {@code msvcrt} file function, read the same way {@link Errno}
 * reads it to build a failure message.
 *
 * @since 0.75.0
 */
public final class ErrnoFuncCall implements Syscall {

    /**
     * Win32 object.
     */
    private final Phi win;

    /**
     * Ctor.
     *
     * @param win Win32 object
     */
    public ErrnoFuncCall(final Phi win) {
        this.win = win;
    }

    @Override
    public Phi make(final Phi... params) {
        final Phi result = this.win.take("return").copy();
        result.put(0, new Data.ToPhi(Msvcrt.INSTANCE._errno().getInt(0)));
        result.put(1, new PhDefault());
        return result;
    }
}
