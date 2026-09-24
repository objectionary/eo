/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import org.eolang.posix.CStdLib;
import org.eolang.sys.Cstring;

/**
 * Tells the value of an environment variable, as POSIX `getenv` does.
 *
 * <p>The code says whether the variable is set at all, since an empty value
 * and a missing variable are not the same thing; the value of a missing one is
 * an empty text.</p>
 *
 * @since 0.77.0
 * @checkstyle IllegalIdentifierNameCheck (6 lines)
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "posix.getenv")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOposix$EOgetenv extends PhDefault implements Atom {

    /**
     * Ctor.
     */
    public EOposix$EOgetenv() {
        super(new Attrs(new Attr("name", new AtVoid("name"))));
    }

    @Override
    public Phi lambda() {
        final String name = new Cstring(Expect.at(this, "name")).it();
        final String env = CStdLib.INSTANCE.getenv(name);
        final Phi result = Phi.Φ.take("posix").take("getenv-return").copy();
        result.put(0, new Data.ToPhi(env != null));
        if (env == null) {
            result.put(1, new Data.ToPhi(""));
        } else {
            result.put(1, new Data.ToPhi(env));
        }
        return result;
    }
}
