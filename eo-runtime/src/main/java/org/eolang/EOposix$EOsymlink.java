/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import org.eolang.posix.CStdLib;
import org.eolang.posix.Errno;
import org.eolang.sys.Cstring;

/**
 * Makes a symbolic link at a path, leading to the file or the directory at
 * the target, as POSIX `symlink` does.
 *
 * <p>The code is {@code -1} when the call did not work, and then the message
 * says why, read out of {@code errno} before any other native call can
 * overwrite it.</p>
 *
 * @since 0.77.0
 * @checkstyle IllegalIdentifierNameCheck (6 lines)
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "posix.symlink")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOposix$EOsymlink extends PhDefault implements Atom {

    /**
     * Ctor.
     */
    public EOposix$EOsymlink() {
        super(
            new Attrs(
                new Attr("v🌵target", new AtVoid("v🌵target")),
                new Attr("v🌵path", new AtVoid("v🌵path"))
            )
        );
    }

    @Override
    public Phi lambda() {
        final String target = new Cstring(Expect.at(this, "v🌵target")).it();
        final String path = new Cstring(Expect.at(this, "v🌵path")).it();
        final Phi result = Phi.Φ.take("posix").take("status-return").copy();
        final int code = CStdLib.INSTANCE.symlink(target, path);
        result.put(0, new Data.ToPhi(code));
        result.put(1, new Errno(code).get());
        return result;
    }
}
