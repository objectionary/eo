/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.posix;

import org.eolang.Data;
import org.eolang.Phi;
import org.eolang.Syscall;

/**
 * Gettimeofday syscall.
 *
 * @since 0.40
 */
public final class GettimeofdaySyscall implements Syscall {

    /**
     * Posix object.
     */
    private final Phi posix;

    /**
     * Ctor.
     *
     * @param posix Posix object
     */
    public GettimeofdaySyscall(final Phi posix) {
        this.posix = posix;
    }

    @Override
    public Phi make(final Phi... params) {
        final Phi result = this.posix.take("return").copy();
        final Timeval timeval = new Timeval();
        result.put(0, new Data.ToPhi(CStdLib.INSTANCE.gettimeofday(timeval, null)));
        final Phi struct = this.posix.take("timeval");
        struct.put(0, new Data.ToPhi(timeval.sec.longValue()));
        struct.put(1, new Data.ToPhi(timeval.usec.longValue()));
        result.put(1, struct);
        return result;
    }
}
