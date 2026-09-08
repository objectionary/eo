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
 * The kernel32 FindNextFileW function call.
 *
 * <p>Reports the next name a search found, with the code {@code 0}. The search
 * running out is a false coming back, and then the code is {@code -1} and there
 * is no name, which is what tells EO to stop reading.</p>
 *
 * @since 0.76.0
 */
public final class FindNextFileFuncCall implements Syscall {

    /**
     * Win32 object.
     */
    private final Phi win;

    /**
     * Ctor.
     * @param win Win32 object
     */
    public FindNextFileFuncCall(final Phi win) {
        this.win = win;
    }

    @Override
    public Phi make(final Phi... params) {
        final String subject = "the 'search' argument of FindNextFile";
        final int handle = new Int(subject, params[0]).it();
        final Phi result = this.win.take("return").copy();
        final Pointer search = Handles.INSTANCE.get(subject, handle);
        final WinFindData data = new WinFindData();
        final boolean found = Kernel32.INSTANCE.FindNextFileW(search, data);
        final int code;
        final Phi output;
        if (found) {
            code = 0;
            output = new Data.ToPhi(data.filename());
        } else {
            code = -1;
            output = new PhDefault();
        }
        result.put(0, new Data.ToPhi(code));
        result.put(1, output);
        return result;
    }
}
