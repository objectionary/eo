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
import java.util.Arrays;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.Phi;

/**
 * ReadFile kernel32 function call.
 * @see <a href="https://learn.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-readfile">here for details</a>
 * @since 0.40.0
 */
public final class ReadFileFuncCall implements Syscall {
    /**
     * Win32 object.
     */
    private final Phi win;

    /**
     * Ctor.
     * @param win Win32 object
     */
    public ReadFileFuncCall(final Phi win) {
        this.win = win;
    }

    @Override
    public Phi make(final Phi... params) {
        final int size = new Dataized(params[1]).asNumber().intValue();
        final byte[] buf = new byte[(int) size];
        final IntByReference read = new IntByReference();
        final Phi result = this.win.take("return").copy();
        result.put(
            0,
            new Data.ToPhi(
                Kernel32.INSTANCE.ReadFile(
                    Kernel32.INSTANCE.GetStdHandle(new Dataized(params[0]).asNumber().intValue()),
                    buf,
                    size,
                    read,
                    null
                )
            )
        );
        result.put(1, new Data.ToPhi(Arrays.copyOf(buf, read.getValue())));
        return result;
    }
}
