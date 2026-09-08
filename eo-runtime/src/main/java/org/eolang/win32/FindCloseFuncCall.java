/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.win32;

import com.sun.jna.Pointer;
import org.eolang.Data;
import org.eolang.Handles;
import org.eolang.Int;
import org.eolang.PhDefault;
import org.eolang.Phi;
import org.eolang.Syscall;

/**
 * The kernel32 FindClose function call.
 *
 * <p>Closes a search and lets {@link Handles} forget the handle, so that the
 * number EO was carrying stops naming anything and a second close of the same
 * search is refused before it reaches the kernel.</p>
 *
 * @since 0.76.0
 */
public final class FindCloseFuncCall implements Syscall {

    /**
     * Win32 object.
     */
    private final Phi win;

    /**
     * Ctor.
     * @param win Win32 object
     */
    public FindCloseFuncCall(final Phi win) {
        this.win = win;
    }

    @Override
    public Phi make(final Phi... params) {
        final String subject = "the 'search' argument of FindClose";
        final int handle = new Int(subject, params[0]).it();
        final Phi result = this.win.take("return").copy();
        final Pointer search = Handles.INSTANCE.remove(subject, handle);
        final int code;
        if (Kernel32.INSTANCE.FindClose(search)) {
            code = 0;
        } else {
            code = -1;
        }
        result.put(0, new Data.ToPhi(code));
        result.put(1, new PhDefault());
        return result;
    }
}
