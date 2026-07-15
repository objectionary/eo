/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
/*
 * @checkstyle PackageNameCheck (4 lines)
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package org.eolang.EO_sm.Posix; // NOPMD

import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.EO_sm.Syscall;
import org.eolang.PhDefault;
import org.eolang.Phi;

/**
 * Mkdir syscall.
 * @since 0.74.0
 */
public final class MkdirSyscall implements Syscall {

    /**
     * Posix object.
     */
    private final Phi posix;

    /**
     * Ctor.
     * @param posix Posix object
     */
    public MkdirSyscall(final Phi posix) {
        this.posix = posix;
    }

    @Override
    public Phi make(final Phi... params) {
        final Phi result = this.posix.take("return").copy();
        result.put(
            0,
            new Data.ToPhi(
                CStdLib.INSTANCE.mkdir(
                    new Dataized(params[0]).asString(),
                    new Dataized(params[1]).asNumber().intValue()
                )
            )
        );
        result.put(1, new PhDefault());
        return result;
    }
}
