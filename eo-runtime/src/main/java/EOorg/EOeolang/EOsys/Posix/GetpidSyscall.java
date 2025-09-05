/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
/*
 * @checkstyle PackageNameCheck (4 lines)
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package EOorg.EOeolang.EOsys.Posix; // NOPMD

import EOorg.EOeolang.EOsys.Syscall;
import org.eolang.Data;
import org.eolang.PhDefault;
import org.eolang.Phi;

/**
 * Getpid syscall.
 * @since 0.40
 */
public final class GetpidSyscall implements Syscall {
    /**
     * Posix object.
     */
    private final Phi posix;

    /**
     * Ctor.
     * @param posix Posix object
     */
    public GetpidSyscall(final Phi posix) {
        this.posix = posix;
    }

    @Override
    public Phi make(final Phi... params) {
        final Phi result = this.posix.take("return").copy();
        result.put(0, new Data.ToPhi(CStdLib.INSTANCE.getpid()));
        result.put(1, new PhDefault());
        return result;
    }
}
