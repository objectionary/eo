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
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.Phi;

/**
 * Getenv syscall.
 * @since 0.40
 */
public final class GetenvSyscall implements Syscall {
    /**
     * Posix object.
     */
    private final Phi posix;

    /**
     * Ctor.
     * @param posix Posix object
     */
    public GetenvSyscall(final Phi posix) {
        this.posix = posix;
    }

    @Override
    public Phi make(final Phi... params) {
        final Phi result = this.posix.take("return").copy();
        final String env = CStdLib.INSTANCE.getenv(new Dataized(params[0]).asString());
        final boolean present = env != null;
        result.put(0, new Data.ToPhi(present));
        if (present) {
            result.put(1, new Data.ToPhi(env));
        } else {
            result.put(1, new Data.ToPhi(""));
        }
        return result;
    }
}
