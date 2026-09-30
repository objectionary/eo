/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import com.sun.jna.Pointer;
import org.eolang.sys.Handles;
import org.eolang.sys.win32.Kernel32;
import org.eolang.sys.win32.WinFindData;

/**
 * Reports the next name a directory search found, as kernel32
 * `FindNextFileW` does.
 *
 * <p>The name comes with the code {@code 0}. The search running out is a
 * false coming back, and then the code is {@code -1} and there is no name,
 * which is what tells EO to stop reading.</p>
 *
 * @since 0.77.0
 * @checkstyle IllegalIdentifierNameCheck (6 lines)
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "win32.find-next-file")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOwin32$EOfind_next_file extends PhDefault implements Atom {

    /**
     * Ctor.
     */
    public EOwin32$EOfind_next_file() {
        super(new Attrs(new Attr("search", new AtVoid("search"))));
    }

    @Override
    public Phi lambda() {
        final Pointer search = Handles.INSTANCE.get(
            "the 'search' attribute",
            new Int(Expect.at(this, "search")).it()
        );
        final WinFindData data = new WinFindData();
        final boolean found = Kernel32.INSTANCE.FindNextFileW(search, data);
        final Phi result = Phi.Φ.take("win32").take("dir-return").copy();
        if (found) {
            result.put(0, new Data.ToPhi(0));
            result.put(1, new Data.ToPhi(data.filename()));
        } else {
            result.put(0, new Data.ToPhi(-1));
            result.put(1, new PhDefault());
        }
        return result;
    }
}
