/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
/*
 * @checkstyle PackageNameCheck (4 lines)
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package EOorg.EOeolang.EOsm.Posix; // NOPMD

import EOorg.EOeolang.EOsm.SockaddrIn;
import EOorg.EOeolang.EOsm.Syscall;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.PhDefault;
import org.eolang.Phi;

/**
 * Bind syscall.
 * @since 0.40
 */
public final class BindSyscall implements Syscall {
    /**
     * Posix object.
     */
    private final Phi posix;

    /**
     * Ctor.
     * @param posix Posix object
     */
    public BindSyscall(final Phi posix) {
        this.posix = posix;
    }

    @Override
    public Phi make(final Phi... params) {
        final Phi result = this.posix.take("return").copy();
        result.put(
            0,
            new Data.ToPhi(
                CStdLib.INSTANCE.bind(
                    new Dataized(params[0]).asNumber().intValue(),
                    new SockaddrIn(
                        new Dataized(params[1].take("sin-family")).take(Short.class),
                        new Dataized(params[1].take("sin-port")).take(Short.class),
                        new Dataized(params[1].take("sin-addr")).take(Integer.class),
                        new Dataized(params[1].take("sin-zero")).take()
                    ),
                    new Dataized(params[2]).asNumber().intValue()
                )
            )
        );
        result.put(1, new PhDefault());
        return result;
    }
}
