/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.posix;

import org.eolang.Cstring;
import org.eolang.Data;
import org.eolang.Phi;
import org.eolang.Syscall;

/**
 * Getenv syscall.
 *
 * @since 0.40
 */
public final class GetenvSyscall implements Syscall {

    /**
     * Posix object.
     */
    private final Phi posix;

    /**
     * Ctor.
     *
     * @param posix Posix object
     */
    public GetenvSyscall(final Phi posix) {
        this.posix = posix;
    }

    @Override
    public Phi make(final Phi... params) {
        final String name = new Cstring("the 'name' argument of getenv", params[0]).it();
        final Phi result = this.posix.take("return").copy();
        final String env = CStdLib.INSTANCE.getenv(name);
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
