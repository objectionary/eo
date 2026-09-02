/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.win32;

import com.sun.jna.Pointer;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.ExFailure;
import org.eolang.Expect;
import org.eolang.Handle;
import org.eolang.Int;
import org.eolang.Natural;
import org.eolang.PhDefault;
import org.eolang.Phi;
import org.eolang.Syscall;

/**
 * WriteFile kernel32 function call.
 * @see <a href="https://learn.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-writefile">here for details</a>
 * @since 0.40.0
 */
public final class SendFuncCall implements Syscall {

    /**
     * Win32 object.
     */
    private final Phi win;

    /**
     * Ctor.
     * @param win Win32 object
     */
    public SendFuncCall(final Phi win) {
        this.win = win;
    }

    @Override
    public Phi make(final Phi... params) {
        final int flags = new Int(
            "the 'flags' argument of send", params[3]
        ).it();
        final byte[] buf = new Dataized(params[1]).take();
        final int size = new Natural(
            new Expect<>("the 'size' argument of send", () -> params[2])
        ).it();
        if (size > buf.length) {
            throw new ExFailure(
                "Can't send %d bytes from a buffer of only %d bytes",
                size, buf.length
            );
        }
        final Phi result = this.win.take("return").copy();
        result.put(
            0,
            new Data.ToPhi(
                Winsock.INSTANCE.send(
                    new Pointer(new Handle("the socket of send", params[0]).it()),
                    buf,
                    size,
                    flags
                )
            )
        );
        result.put(1, new PhDefault());
        return result;
    }
}
