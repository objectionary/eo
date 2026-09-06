/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.posix;

import java.util.Arrays;
import org.eolang.Data;
import org.eolang.Expect;
import org.eolang.Int;
import org.eolang.Natural;
import org.eolang.Phi;
import org.eolang.Syscall;

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
        final int size = new Natural(
            new Expect<>("the 'size' argument of recv", () -> params[1])
        ).it();
        final byte[] buf = new Buffer("the 'size' argument of recv", size).it();
        final int received = CStdLib.INSTANCE.recv(
            new Int("the 'descriptor' argument of recv", params[0]).it(),
            buf,
            size,
            new Int("the 'flags' argument of recv", params[2]).it()
        );
        result.put(0, new Data.ToPhi(received));
        result.put(1, new Data.ToPhi(Arrays.copyOf(buf, Math.max(received, 0))));
        return result;
    }
}
