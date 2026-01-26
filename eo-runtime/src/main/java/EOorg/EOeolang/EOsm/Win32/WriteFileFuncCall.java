/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

/*
 * @checkstyle PackageNameCheck (4 lines)
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package EOorg.EOeolang.EOsm.Win32; // NOPMD

import EOorg.EOeolang.EOsm.Syscall;
import com.sun.jna.ptr.IntByReference;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.Phi;

/**
 * WriteFile kernel32 function call.
 * @see <a href="https://learn.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-writefile">here for details</a>
 * @since 0.40.0
 */
public final class WriteFileFuncCall implements Syscall {
    /**
     * Win32 object.
     */
    private final Phi win;

    /**
     * Ctor.
     * @param win Win32 object
     */
    public WriteFileFuncCall(final Phi win) {
        this.win = win;
    }

    @Override
    public Phi make(final Phi... params) {
        final IntByReference written = new IntByReference();
        final Phi result = this.win.take("return").copy();
        result.put(
            "code",
            new Data.ToPhi(
                Kernel32.INSTANCE.WriteFile(
                    Kernel32.INSTANCE.GetStdHandle(
                        new Dataized(params[0]).asNumber().intValue()
                    ),
                    new Dataized(params[1]).take(),
                    new Dataized(params[2]).asNumber().intValue(),
                    written,
                    null
                )
            )
        );
        result.put("output", new Data.ToPhi(written.getValue()));
        return result;
    }
}
