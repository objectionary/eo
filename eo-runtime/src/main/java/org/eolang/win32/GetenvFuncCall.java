/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.win32;

import org.eolang.Cstring;
import org.eolang.Data;
import org.eolang.Phi;
import org.eolang.Syscall;

/**
 * The msvcrt getenv function call.
 *
 * @since 0.74.0
 */
public final class GetenvFuncCall implements Syscall {

    /**
     * Win32 object.
     */
    private final Phi win;

    /**
     * Ctor.
     *
     * @param win Win32 object
     */
    public GetenvFuncCall(final Phi win) {
        this.win = win;
    }

    @Override
    public Phi make(final Phi... params) {
        final String name = new Cstring("the 'name' argument of getenv", params[0]).it();
        final Phi result = this.win.take("return").copy();
        final String env = Msvcrt.INSTANCE.getenv(name);
        final boolean present = env != null;
        result.put(0, new Data.ToPhi(present));
        if (present) {
            result.put(1, new Data.ToPhi(env));
        } else {
            result.put(1, new Data.ToPhi(""));
        }
        return result;
    }
}
