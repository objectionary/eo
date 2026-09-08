/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.posix;

import com.sun.jna.Pointer;
import org.eolang.Data;
import org.eolang.Handles;
import org.eolang.Int;
import org.eolang.Phi;
import org.eolang.Syscall;

/**
 * Closedir syscall.
 *
 * <p>Closes a directory stream and lets {@link Handles} forget the pointer,
 * so that the number EO was carrying stops naming anything and a second close
 * of the same stream is refused before it reaches libc.</p>
 *
 * @since 0.76.0
 */
public final class ClosedirSyscall implements Syscall {

    /**
     * Posix object.
     */
    private final Phi posix;

    /**
     * Ctor.
     *
     * @param posix Posix object
     */
    public ClosedirSyscall(final Phi posix) {
        this.posix = posix;
    }

    @Override
    public Phi make(final Phi... params) {
        final String subject = "the 'dirp' argument of closedir";
        final Pointer stream = Handles.INSTANCE.remove(
            subject, new Int(subject, params[0]).it()
        );
        final Phi result = this.posix.take("return").copy();
        final int code = CStdLib.INSTANCE.closedir(stream);
        result.put(0, new Data.ToPhi(code));
        result.put(1, new Errno(code).get());
        return result;
    }
}
