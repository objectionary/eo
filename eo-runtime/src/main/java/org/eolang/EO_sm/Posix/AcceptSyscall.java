/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
/*
 * @checkstyle PackageNameCheck (4 lines)
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package org.eolang.EO_sm.Posix; // NOPMD

import com.sun.jna.ptr.IntByReference;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.EO_sm.SockaddrIn;
import org.eolang.EO_sm.Syscall;
import org.eolang.PhDefault;
import org.eolang.Phi;

/**
 * Accept syscall.
 * @since 0.40
 */
public final class AcceptSyscall implements Syscall {

    /**
     * Posix object.
     */
    private final Phi posix;

    /**
     * Ctor.
     * @param posix Posix object
     */
    public AcceptSyscall(final Phi posix) {
        this.posix = posix;
    }

    @Override
    public Phi make(final Phi... params) {
        final Phi result = this.posix.take("return").copy();
        result.put(
            0,
            new Data.ToPhi(
                CStdLib.INSTANCE.accept(
                    new Dataized(params[0]).asNumber().intValue(),
                    new SockaddrIn(
                        new Dataized(params[1].take("sinfamily")).take(Short.class),
                        new Dataized(params[1].take("sinport")).take(Short.class),
                        new Dataized(params[1].take("sinaddr")).take(Integer.class),
                        new Dataized(params[1].take("sinzero")).take()
                    ),
                    new IntByReference(new Dataized(params[2]).asNumber().intValue())
                )
            )
        );
        result.put(1, new PhDefault());
        return result;
    }
}
