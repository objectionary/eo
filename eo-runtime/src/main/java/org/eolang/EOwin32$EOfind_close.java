/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import org.eolang.sys.Handles;
import org.eolang.sys.win32.Kernel32;

/**
 * Ends a directory search, as kernel32 `FindClose` does.
 *
 * <p>The handle is forgotten here as well, so the number EO was carrying stops
 * naming anything and a second close of the same search is refused before it
 * reaches the kernel.</p>
 *
 * @since 0.77.0
 * @checkstyle IllegalIdentifierNameCheck (6 lines)
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "win32.find-close")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOwin32$EOfind_close extends PhDefault implements Atom {

    /**
     * Ctor.
     */
    public EOwin32$EOfind_close() {
        super(new Attrs(new Attr("search", new AtVoid("search"))));
    }

    @Override
    public Phi lambda() {
        final int code;
        if (
            Kernel32.INSTANCE.FindClose(
                Handles.INSTANCE.remove(
                    "the 'search' attribute",
                    new Int(Expect.at(this, "search")).it()
                )
            )
        ) {
            code = 0;
        } else {
            code = -1;
        }
        return new Data.ToPhi(code);
    }
}
