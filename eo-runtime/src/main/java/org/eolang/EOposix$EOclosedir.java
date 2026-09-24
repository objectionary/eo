/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import com.sun.jna.Pointer;
import org.eolang.posix.CStdLib;
import org.eolang.posix.Errno;
import org.eolang.sys.Handles;

/**
 * Closes a directory stream, as POSIX `closedir` does.
 *
 * <p>The code is {@code -1} when the call did not work, and then the message
 * says why, read out of {@code errno} before any other native call can
 * overwrite it.</p>
 *
 * <p>The handle is forgotten here as well, so the number EO was carrying
 * stops naming anything and a second close of the same stream is refused
 * before it reaches libc.</p>
 *
 * @since 0.77.0
 * @checkstyle IllegalIdentifierNameCheck (6 lines)
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "posix.closedir")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOposix$EOclosedir extends PhDefault implements Atom {

    /**
     * Ctor.
     */
    public EOposix$EOclosedir() {
        super(new Attrs(new Attr("dirp", new AtVoid("dirp"))));
    }

    @Override
    public Phi lambda() {
        final Pointer stream = Handles.INSTANCE.remove(
            "the 'dirp' attribute",
            new Int(Expect.at(this, "dirp")).it()
        );
        final Phi result = Phi.Φ.take("posix").take("status-return").copy();
        final int code = CStdLib.INSTANCE.closedir(stream);
        result.put(0, new Data.ToPhi(code));
        result.put(1, new Errno(code).get());
        return result;
    }
}
