/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
/*
 * @checkstyle PackageNameCheck (4 lines)
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package org.eolang.Win32; // NOPMD

import java.util.Arrays;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.Phi;
import org.eolang.Syscall;

/**
 * The msvcrt _read function call.
 * @since 0.74.0
 */
public final class ReadFuncCall implements Syscall {

    /**
     * Win32 object.
     */
    private final Phi win;

    /**
     * Ctor.
     * @param win Win32 object
     */
    public ReadFuncCall(final Phi win) {
        this.win = win;
    }

    @Override
    public Phi make(final Phi... params) {
        final int size = new Dataized(params[1]).asNumber().intValue();
        final byte[] buf = new byte[size];
        final int count = Msvcrt.INSTANCE._read(
            new Dataized(params[0]).asNumber().intValue(), buf, size
        );
        final Phi result = this.win.take("return").copy();
        result.put(0, new Data.ToPhi(count));
        result.put(1, new Data.ToPhi(Arrays.copyOf(buf, Math.max(count, 0))));
        return result;
    }
}
