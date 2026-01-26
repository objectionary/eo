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
import java.util.Arrays;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.Phi;

/**
 * Recv syscall.
 * @since 0.40
 */
public final class RecvSyscall implements Syscall {
    /**
     * Posix object.
     */
    private final Phi posix;

    /**
     * Ctor.
     * @param posix Posix object
     */
    public RecvSyscall(final Phi posix) {
        this.posix = posix;
    }

    @Override
    public Phi make(final Phi... params) {
        final Phi result = this.posix.take("return").copy();
        final int size = new Dataized(params[1]).asNumber().intValue();
        final byte[] buf = new byte[(int) size];
        final int received = CStdLib.INSTANCE.recv(
            new Dataized(params[0]).asNumber().intValue(),
            buf,
            size,
            new Dataized(params[2]).asNumber().intValue()
        );
        result.put(0, new Data.ToPhi(received));
        result.put(1, new Data.ToPhi(Arrays.copyOf(buf, received)));
        return result;
    }
}
