/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import com.sun.jna.Native;
import com.sun.jna.WString;
import org.eolang.sys.Cstring;
import org.eolang.sys.win32.Kernel32;

/**
 * Reports the attribute flags a path carries, as kernel32
 * `GetFileAttributesW` does.
 *
 * <p>The code is the attribute bitmask returned by {@code GetFileAttributesW},
 * or {@code -1} ({@code INVALID_FILE_ATTRIBUTES}) on failure. On success the
 * message is an empty string; on failure the message carries the error code
 * reported by {@code GetLastError}.</p>
 *
 * @since 0.77.0
 * @checkstyle IllegalIdentifierNameCheck (6 lines)
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "win32.get-file-attributes")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOwin32$EOget_file_attributes extends PhDefault implements Atom {

    /**
     * Ctor.
     */
    public EOwin32$EOget_file_attributes() {
        super(new Attrs(new Attr("path")));
    }

    @Override
    public Phi lambda() {
        final String path = new Cstring(this.expected("path")).it();
        final Phi result = Phi.Φ.take("win32").take("status-return").copy();
        final int code = Kernel32.INSTANCE.GetFileAttributesW(new WString(path));
        result.put(0, new Data.ToPhi(code));
        if (code == -1) {
            result.put(
                1,
                new Data.ToPhi(String.format("Win32 error %d", Native.getLastError()))
            );
        } else {
            result.put(1, new Data.ToPhi(""));
        }
        return result;
    }
}
