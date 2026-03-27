/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

/*
 * @checkstyle PackageNameCheck (4 lines)
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package org.eolang.EOms; // NOPMD

import org.eolang.AtVoid;
import org.eolang.Atom;
import org.eolang.Dataized;
import org.eolang.PhDefault;
import org.eolang.Phi;
import org.eolang.XmirObject;

/**
 * Asin.
 *
 * @since 0.40
 * @checkstyle TypeNameCheck (100 lines)
 */
@XmirObject(oname = "asin")
public final class EOasin extends PhDefault implements Atom {
    /**
     * Ctor.
     */
    @SuppressWarnings("PMD.ConstructorOnlyInitializesOrCallOtherConstructors")
    public EOasin() {
        this.add("num", new AtVoid("num"));
    }

    @Override
    public Phi lambda() {
        return new ToPhi(
            Math.asin(new Dataized(this.take("num")).asNumber())
        );
    }
}
