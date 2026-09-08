/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.win32;

import java.util.Arrays;
import org.eolang.Data;
import org.eolang.Expect;
import org.eolang.Int;
import org.eolang.Natural;
import org.eolang.Phi;
import org.eolang.Syscall;

/**
 * The msvcrt _read function call.
 *
 * @since 0.74.0
 */
public final class ReadFuncCall implements Syscall {

    /**
     * Win32 object.
     */
    private final Phi win;

    /**
     * Ctor.
     *
     * @param win Win32 object
     */
    public ReadFuncCall(final Phi win) {
        this.win = win;
    }

    @Override
    public Phi make(final Phi... params) {
        final int descriptor = new Int(
            "the 'descriptor' argument of read", params[0]
        ).it();
        final int size = new Natural(
            new Expect<>("the 'size' argument of read", () -> params[1])
        ).it();
        final byte[] buf = new byte[size];
        final int count = Msvcrt.INSTANCE._read(
            descriptor, buf, size
        );
        final Phi result = this.win.take("return").copy();
        result.put(0, new Data.ToPhi(count));
        result.put(1, new Data.ToPhi(Arrays.copyOf(buf, Math.max(count, 0))));
        return result;
    }
}
