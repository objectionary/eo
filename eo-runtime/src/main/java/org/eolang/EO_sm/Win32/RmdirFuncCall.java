/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
/*
 * @checkstyle PackageNameCheck (4 lines)
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package org.eolang.EO_sm.Win32; // NOPMD

import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.EO_sm.Syscall;
import org.eolang.PhDefault;
import org.eolang.Phi;

/**
 * The msvcrt _rmdir function call.
 * @since 0.74.0
 */
public final class RmdirFuncCall implements Syscall {

    /**
     * Win32 object.
     */
    private final Phi win;

    /**
     * Ctor.
     * @param win Win32 object
     */
    public RmdirFuncCall(final Phi win) {
        this.win = win;
    }

    @Override
    public Phi make(final Phi... params) {
        final Phi result = this.win.take("return").copy();
        result.put(
            0,
            new Data.ToPhi(
                Msvcrt.INSTANCE._rmdir(new Dataized(params[0]).asString())
            )
        );
        result.put(1, new PhDefault());
        return result;
    }
}
