/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.win32;

import com.sun.jna.Pointer;
import org.eolang.Data;
import org.eolang.Handle;
import org.eolang.Int;
import org.eolang.PhDefault;
import org.eolang.Phi;
import org.eolang.Syscall;

/**
 * WriteFile kernel32 function call.
 * @see <a href="https://learn.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-writefile">here for details</a>
 * @since 0.40.0
 */
public final class ListenFuncCall implements Syscall {

    /**
     * Win32 object.
     */
    private final Phi win;

    /**
     * Ctor.
     * @param win Win32 object
     */
    public ListenFuncCall(final Phi win) {
        this.win = win;
    }

    @Override
    public Phi make(final Phi... params) {
        final int backlog = new Int(
            "the 'backlog' argument of listen", params[1]
        ).it();
        final Phi result = this.win.take("return").copy();
        result.put(
            0,
            new Data.ToPhi(
                Winsock.INSTANCE.listen(
                    new Pointer(new Handle("the socket of listen", params[0]).it()),
                    backlog
                )
            )
        );
        result.put(1, new PhDefault());
        return result;
    }
}
