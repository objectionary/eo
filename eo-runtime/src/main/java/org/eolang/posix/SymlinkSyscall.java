/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.posix;

import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.Phi;
import org.eolang.Syscall;

/**
 * Symlink syscall.
 *
 * <p>It creates a symbolic link at the second path, leading to the file or
 * the directory at the first one.</p>
 *
 * @since 0.74.0
 */
public final class SymlinkSyscall implements Syscall {

    /**
     * Posix object.
     */
    private final Phi posix;

    /**
     * Ctor.
     * @param posix Posix object
     */
    public SymlinkSyscall(final Phi posix) {
        this.posix = posix;
    }

    @Override
    public Phi make(final Phi... params) {
        final Phi result = this.posix.take("return").copy();
        final int code = CStdLib.INSTANCE.symlink(
            new Dataized(params[0]).asString(),
            new Dataized(params[1]).asString()
        );
        result.put(0, new Data.ToPhi(code));
        result.put(1, new Errno(code).get());
        return result;
    }
}
