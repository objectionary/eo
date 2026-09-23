/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import org.eolang.posix.CStdLib;
import org.eolang.posix.Stat;
import org.eolang.sys.Cstring;

/**
 * Tells the mode bits and the size of the file a path leads to, as `stat(2)`
 * does, following a symbolic link to whatever stands behind it.
 *
 * @since 0.77.0
 * @checkstyle IllegalIdentifierNameCheck (6 lines)
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "posix.stat")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOposix$EOstat extends PhDefault implements Atom {

    /**
     * Ctor.
     */
    public EOposix$EOstat() {
        super(new Attrs(new Attr("path", new AtVoid("path"))));
    }

    @Override
    public Phi lambda() {
        return new Stat(
            new Cstring(Expect.at(this, "path")).it(),
            (path, buf) -> CStdLib.INSTANCE.stat(path, buf)
        ).it();
    }
}
