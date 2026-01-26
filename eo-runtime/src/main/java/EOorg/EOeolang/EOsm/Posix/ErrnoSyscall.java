/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
/*
 * @checkstyle PackageNameCheck (4 lines)
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package EOorg.EOeolang.EOsm.Posix; // NOPMD

import EOorg.EOeolang.EOsm.Syscall;
import com.sun.jna.Native;
import org.eolang.Data;
import org.eolang.PhDefault;
import org.eolang.Phi;

/**
 * The 'errno' syscall.
 * @since 0.40
 */
public final class ErrnoSyscall implements Syscall {
    /**
     * Posix object.
     */
    private final Phi posix;

    /**
     * Ctor.
     * @param posix Posix object
     */
    public ErrnoSyscall(final Phi posix) {
        this.posix = posix;
    }

    @Override
    public Phi make(final Phi... params) {
        final Phi result = this.posix.take("return").copy();
        result.put(0, new Data.ToPhi(Native.getLastError()));
        result.put(1, new PhDefault());
        return result;
    }
}
