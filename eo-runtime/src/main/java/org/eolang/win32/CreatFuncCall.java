/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.win32;

import com.sun.jna.WString;
import org.eolang.Cstring;
import org.eolang.Data;
import org.eolang.Int;
import org.eolang.Phi;
import org.eolang.Syscall;

/**
 * The msvcrt _creat function call.
 *
 * @since 0.74.0
 */
public final class CreatFuncCall implements Syscall {

    /**
     * Win32 object.
     */
    private final Phi win;

    /**
     * Ctor.
     *
     * @param win Win32 object
     */
    public CreatFuncCall(final Phi win) {
        this.win = win;
    }

    @Override
    public Phi make(final Phi... params) {
        final int mode = new Int(
            "the 'mode' argument of creat", params[1]
        ).it();
        final String path = new Cstring("the 'path' argument of creat", params[0]).it();
        final Phi result = this.win.take("return").copy();
        final int code = Msvcrt.INSTANCE._wcreat(
            new WString(path),
            mode
        );
        result.put(0, new Data.ToPhi(code));
        result.put(1, new Errno(code).get());
        return result;
    }
}
