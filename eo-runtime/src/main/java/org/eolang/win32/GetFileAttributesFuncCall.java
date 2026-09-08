/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.win32;

import com.sun.jna.WString;
import org.eolang.Cstring;
import org.eolang.Data;
import org.eolang.PhDefault;
import org.eolang.Phi;
import org.eolang.Syscall;

/**
 * The kernel32 GetFileAttributesW function call.
 *
 * @since 0.75.0
 */
public final class GetFileAttributesFuncCall implements Syscall {

    /**
     * Win32 object.
     */
    private final Phi win;

    /**
     * Ctor.
     *
     * @param win Win32 object
     */
    public GetFileAttributesFuncCall(final Phi win) {
        this.win = win;
    }

    @Override
    public Phi make(final Phi... params) {
        final String path = new Cstring(
            "the 'path' argument of GetFileAttributes", params[0]
        ).it();
        final Phi result = this.win.take("return").copy();
        result.put(
            0,
            new Data.ToPhi(Kernel32.INSTANCE.GetFileAttributesW(new WString(path)))
        );
        result.put(1, new PhDefault());
        return result;
    }
}
