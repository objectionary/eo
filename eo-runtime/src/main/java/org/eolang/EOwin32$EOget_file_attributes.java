/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import com.sun.jna.WString;
import org.eolang.sys.Cstring;
import org.eolang.sys.win32.Kernel32;

/**
 * Reports the attribute flags a path carries, as kernel32
 * `GetFileAttributesW` does.
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
        super(new Attrs(new Attr("path", new AtVoid("path"))));
    }

    @Override
    public Phi lambda() {
        return new Data.ToPhi(
            Kernel32.INSTANCE.GetFileAttributesW(
                new WString(new Cstring(Expect.at(this, "path")).it())
            )
        );
    }
}
