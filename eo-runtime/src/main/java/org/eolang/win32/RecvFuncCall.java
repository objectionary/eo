/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.win32;

import com.sun.jna.Pointer;
import java.util.Arrays;
import org.eolang.Data;
import org.eolang.Expect;
import org.eolang.Handle;
import org.eolang.Int;
import org.eolang.Natural;
import org.eolang.Phi;
import org.eolang.Syscall;

/**
 * ReadFile kernel32 function call.
 * @see <a href="https://learn.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-readfile">here for details</a>
 * @since 0.40.0
 */
public final class RecvFuncCall implements Syscall {

    /**
     * Win32 object.
     */
    private final Phi win;

    /**
     * Ctor.
     * @param win Win32 object
     */
    public RecvFuncCall(final Phi win) {
        this.win = win;
    }

    @Override
    public Phi make(final Phi... params) {
        final int flags = new Int(
            "the 'flags' argument of recv", params[2]
        ).it();
        final Phi result = this.win.take("return").copy();
        final int size = new Natural(
            new Expect<>("the 'size' argument of recv", () -> params[1])
        ).it();
        final byte[] buf = new byte[size];
        final int received = Winsock.INSTANCE.recv(
            new Pointer(new Handle("the socket of recv", params[0]).it()),
            buf,
            size,
            flags
        );
        result.put(0, new Data.ToPhi(received));
        result.put(1, new Data.ToPhi(Arrays.copyOf(buf, Math.max(received, 0))));
        return result;
    }
}
