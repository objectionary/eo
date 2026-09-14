/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.posix;

import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.ExFailure;
import org.eolang.Expect;
import org.eolang.Int;
import org.eolang.Natural;
import org.eolang.PhDefault;
import org.eolang.Phi;
import org.eolang.Syscall;

/**
 * Write syscall.
 *
 * @since 0.40
 */
public final class WriteSyscall implements Syscall {

    /**
     * Posix object.
     */
    private final Phi posix;

    /**
     * Ctor.
     *
     * @param posix Posix object
     */
    public WriteSyscall(final Phi posix) {
        this.posix = posix;
    }

    @Override
    public Phi make(final Phi... params) {
        final byte[] buf = new Dataized(params[1]).take();
        final int size = new Natural(
            new Expect<>("the 'size' argument of write", () -> params[2])
        ).it();
        if (size > buf.length) {
            throw new ExFailure(
                "Can't write %d bytes from a buffer of only %d bytes",
                size, buf.length
            );
        }
        final Phi result = this.posix.take("return").copy();
        result.put(
            0,
            new Data.ToPhi(
                CStdLib.INSTANCE.write(
                    new Int("the 'descriptor' argument of write", params[0]).it(),
                    buf,
                    size
                )
            )
        );
        result.put(1, new PhDefault());
        return result;
    }
}
