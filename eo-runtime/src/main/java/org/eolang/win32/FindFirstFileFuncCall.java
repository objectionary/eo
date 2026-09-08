/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.win32;

import com.sun.jna.Native;
import com.sun.jna.Pointer;
import com.sun.jna.WString;
import org.eolang.Cstring;
import org.eolang.Data;
import org.eolang.Handles;
import org.eolang.Phi;
import org.eolang.Syscall;

/**
 * The kernel32 FindFirstFileW function call.
 *
 * <p>Starts a search and hands EO the number under which {@link Handles} keeps
 * the handle it got back, together with the first name the search found: on
 * Windows the search opens and reads at once, unlike {@code opendir}, so the
 * first entry has nowhere else to go. A failure is a code of {@code -1}, with
 * the number the kernel left behind in the output.</p>
 *
 * @since 0.76.0
 */
public final class FindFirstFileFuncCall implements Syscall {

    /**
     * Win32 object.
     */
    private final Phi win;

    /**
     * Ctor.
     * @param win Win32 object
     */
    public FindFirstFileFuncCall(final Phi win) {
        this.win = win;
    }

    @Override
    public Phi make(final Phi... params) {
        final String pattern = new Cstring(
            "the 'pattern' argument of FindFirstFile", params[0]
        ).it();
        final Phi result = this.win.take("return").copy();
        final WinFindData data = new WinFindData();
        final Pointer search = Kernel32.INSTANCE.FindFirstFileW(new WString(pattern), data);
        final int code;
        final Phi output;
        if (search == null || Pointer.nativeValue(search) == -1L) {
            code = -1;
            output = new Data.ToPhi(
                String.format("Win32 error %d", Native.getLastError())
            );
        } else {
            code = Handles.INSTANCE.add(search);
            output = new Data.ToPhi(data.filename());
        }
        result.put(0, new Data.ToPhi(code));
        result.put(1, output);
        return result;
    }
}
