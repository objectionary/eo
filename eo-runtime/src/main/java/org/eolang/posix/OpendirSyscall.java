/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.posix;

import com.sun.jna.Pointer;
import org.eolang.Cstring;
import org.eolang.Data;
import org.eolang.Handles;
import org.eolang.Phi;
import org.eolang.Syscall;

/**
 * Opendir syscall.
 *
 * <p>Opens a directory for reading and hands EO the number under which
 * {@link Handles} keeps the {@code DIR*} it got back, since a pointer itself
 * has no safe shape in EO. A {@code NULL} means the directory could not be
 * opened, and then the number is {@code -1} and the reason comes along in the
 * output, the way every other file syscall reports a failure.</p>
 *
 * @since 0.76.0
 */
public final class OpendirSyscall implements Syscall {

    /**
     * Posix object.
     */
    private final Phi posix;

    /**
     * Ctor.
     * @param posix Posix object
     */
    public OpendirSyscall(final Phi posix) {
        this.posix = posix;
    }

    @Override
    public Phi make(final Phi... params) {
        final String path = new Cstring("the 'path' argument of opendir", params[0]).it();
        final Phi result = this.posix.take("return").copy();
        final Pointer stream = CStdLib.INSTANCE.opendir(path);
        final int code;
        if (stream == null) {
            code = -1;
        } else {
            code = Handles.INSTANCE.add(stream);
        }
        result.put(0, new Data.ToPhi(code));
        result.put(1, new Errno(code).get());
        return result;
    }
}
