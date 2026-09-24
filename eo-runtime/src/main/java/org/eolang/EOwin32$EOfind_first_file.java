/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import com.sun.jna.Native;
import com.sun.jna.Pointer;
import com.sun.jna.WString;
import org.eolang.sys.Cstring;
import org.eolang.sys.Handles;
import org.eolang.sys.win32.Kernel32;
import org.eolang.sys.win32.WinFindData;

/**
 * Starts a directory search, as kernel32 `FindFirstFileW` does.
 *
 * <p>The code is the number under which {@link Handles} keeps the handle it
 * got back, and the name is the first entry the search found: on Windows the
 * search opens and reads at once, unlike {@code opendir}, so the first entry
 * has nowhere else to go. A failure is a code of {@code -1}, with no name and
 * with the number the kernel left behind in the message.</p>
 *
 * @since 0.77.0
 * @checkstyle IllegalIdentifierNameCheck (6 lines)
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "win32.find-first-file")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOwin32$EOfind_first_file extends PhDefault implements Atom {

    /**
     * Ctor.
     */
    public EOwin32$EOfind_first_file() {
        super(new Attrs(new Attr("pattern", new AtVoid("pattern"))));
    }

    @Override
    public Phi lambda() {
        final String pattern = new Cstring(Expect.at(this, "pattern")).it();
        final Phi result = Phi.Φ.take("win32").take("search-return").copy();
        final WinFindData data = new WinFindData();
        final Pointer search = Kernel32.INSTANCE.FindFirstFileW(new WString(pattern), data);
        if (search == null || Pointer.nativeValue(search) == -1L) {
            result.put(0, new Data.ToPhi(-1));
            result.put(1, new PhDefault());
            result.put(
                2,
                new Data.ToPhi(String.format("Win32 error %d", Native.getLastError()))
            );
        } else {
            result.put(0, new Data.ToPhi(Handles.INSTANCE.add(search)));
            result.put(1, new Data.ToPhi(data.filename()));
            result.put(2, new PhDefault());
        }
        return result;
    }
}
