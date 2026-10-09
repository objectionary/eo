/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import org.eolang.posix.CStdLib;
import org.eolang.posix.Errno;
import org.eolang.sys.Cstring;

/**
 * Removes a name from the file system, as POSIX `unlink` does, which
 * deletes a file or a symbolic link but never a directory.
 *
 * <p>The code is {@code 0} when the call worked and {@code -1} when it did
 * not, and then the message says why, read out of {@code errno} before any
 * other native call can overwrite it.</p>
 *
 * @since 0.77.0
 * @checkstyle IllegalIdentifierNameCheck (6 lines)
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "posix.unlink")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOposix$EOunlink extends PhDefault implements Atom {

    /**
     * Ctor.
     */
    public EOposix$EOunlink() {
        super(new Attrs(new Attr("path", new AtVoid("path"))));
    }

    @Override
    public Phi lambda() {
        final String path = new Cstring(Expect.at(this, "path")).it();
        final Phi result = Phi.Φ.take("posix").take("status-return").copy();
        final int code = CStdLib.INSTANCE.unlink(path);
        result.put(0, new Data.ToPhi(code));
        result.put(1, new Errno(code).get());
        return result;
    }
}
