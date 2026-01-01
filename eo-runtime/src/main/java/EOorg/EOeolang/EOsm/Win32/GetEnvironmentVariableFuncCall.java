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
import java.util.Arrays;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.Phi;

/**
 * GetEnvironmentVariable kernel32 function call.
 * @see <a href="https://learn.microsoft.com/en-us/windows/win32/api/winbase/nf-winbase-getenvironmentvariable">here for details</a>
 * @since 0.40.0
 */
public final class GetEnvironmentVariableFuncCall implements Syscall {
    /**
     * Win32 object.
     */
    private final Phi win;

    /**
     * Ctor.
     * @param win Win32 object
     */
    public GetEnvironmentVariableFuncCall(final Phi win) {
        this.win = win;
    }

    @Override
    public Phi make(final Phi... params) {
        final int size = new Dataized(params[1]).asNumber().intValue();
        final char[] buf = new char[size];
        final int length = Kernel32.INSTANCE.GetEnvironmentVariable(
            new Dataized(params[0]).asString(), buf, size
        );
        final Phi result = this.win.take("return").copy();
        result.put(0, new Data.ToPhi(length));
        if (length > 0) {
            result.put(1, new Data.ToPhi(new String(Arrays.copyOf(buf, length))));
        } else {
            result.put(1, new Data.ToPhi(""));
        }
        return result;
    }
}
