/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import com.sun.jna.Pointer;
import org.eolang.posix.CStdLib;
import org.eolang.posix.Errno;
import org.eolang.sys.Cstring;
import org.eolang.sys.Handles;

/**
 * Opens a directory for reading, as POSIX `opendir` does.
 *
 * <p>The code is the number under which {@link Handles} keeps the
 * {@code DIR*} it got back, since a pointer itself has no safe shape in EO.
 * A {@code NULL} means the directory could not be opened, and then the code
 * is {@code -1} and the message says why, read out of {@code errno} before
 * any other native call can overwrite it.</p>
 *
 * @since 0.77.0
 * @checkstyle IllegalIdentifierNameCheck (6 lines)
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "posix.opendir")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOposix$EOopendir extends PhDefault implements Atom {

    /**
     * Ctor.
     */
    public EOposix$EOopendir() {
        super(new Attrs(new Attr("path", new AtVoid("path"))));
    }

    @Override
    public Phi lambda() {
        final String path = new Cstring(Expect.at(this, "path")).it();
        final Phi result = Phi.Φ.take("posix").take("status-return").copy();
        final Pointer stream = CStdLib.INSTANCE.opendir(path);
        final int code;
        if (stream == null) {
            code = -1;
        } else {
            code = Handles.INSTANCE.add(stream);
        }
        result.put(0, new Data.ToPhi(code));
        result.put(1, new Errno(code).get());
        return result;
    }
}
