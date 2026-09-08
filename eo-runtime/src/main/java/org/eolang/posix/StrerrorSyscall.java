/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.posix;

import org.eolang.Data;
import org.eolang.Int;
import org.eolang.PhDefault;
import org.eolang.Phi;
import org.eolang.Syscall;

/**
 * The 'strerror' syscall.
 *
 * @since 0.40
 */
public final class StrerrorSyscall implements Syscall {

    /**
     * Posix object.
     */
    private final Phi posix;

    /**
     * Ctor.
     *
     * @param posix Posix object
     */
    public StrerrorSyscall(final Phi posix) {
        this.posix = posix;
    }

    @Override
    public Phi make(final Phi... params) {
        final int errno = new Int("the 'errno' argument of strerror", params[0]).it();
        final Phi result = this.posix.take("return").copy();
        result.put(0, new Data.ToPhi(new Strerror(errno).it()));
        result.put(1, new PhDefault());
        return result;
    }
}
