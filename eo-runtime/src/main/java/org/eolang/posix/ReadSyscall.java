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
 * Read syscall.
 * @since 0.40
 */
public final class ReadSyscall implements Syscall {

    /**
     * Posix object.
     */
    private final Phi posix;

    /**
     * Ctor.
     * @param posix Posix object
     */
    public ReadSyscall(final Phi posix) {
        this.posix = posix;
    }

    @Override
    public Phi make(final Phi... params) {
        final int size = new Natural(
            new Expect<>("the 'size' argument of read", () -> params[1])
        ).it();
        final Phi result = this.posix.take("return").copy();
        final byte[] buf = new Buffer("the 'size' argument of read", size).it();
        final int count = CStdLib.INSTANCE.read(
            new Int("the 'descriptor' argument of read", params[0]).it(), buf, size
        );
        result.put(0, new Data.ToPhi(count));
        result.put(1, new Data.ToPhi(Arrays.copyOf(buf, Math.max(count, 0))));
        return result;
    }
}
