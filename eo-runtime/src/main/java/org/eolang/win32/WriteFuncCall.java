/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.win32;

import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.ExFailure;
import org.eolang.PhDefault;
import org.eolang.Phi;
import org.eolang.Syscall;

/**
 * The msvcrt _write function call.
 * @since 0.74.0
 */
public final class WriteFuncCall implements Syscall {

    /**
     * Win32 object.
     */
    private final Phi win;

    /**
     * Ctor.
     * @param win Win32 object
     */
    public WriteFuncCall(final Phi win) {
        this.win = win;
    }

    @Override
    public Phi make(final Phi... params) {
        final byte[] buf = new Dataized(params[1]).take();
        final int size = new Dataized(params[2]).asNumber().intValue();
        if (size < 0 || size > buf.length) {
            throw new ExFailure(
                "Can't write %d bytes from a buffer of only %d bytes",
                size, buf.length
            );
        }
        final Phi result = this.win.take("return").copy();
        result.put(
            0,
            new Data.ToPhi(
                Msvcrt.INSTANCE._write(
                    new Dataized(params[0]).asNumber().intValue(),
                    buf,
                    size
                )
            )
        );
        result.put(1, new PhDefault());
        return result;
    }
}
