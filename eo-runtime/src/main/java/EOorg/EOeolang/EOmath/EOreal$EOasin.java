/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

/*
 * @checkstyle PackageNameCheck (4 lines)
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package EOorg.EOeolang.EOmath; // NOPMD

import org.eolang.Atom;
import org.eolang.Attr;
import org.eolang.Dataized;
import org.eolang.PhDefault;
import org.eolang.Phi;
import org.eolang.XmirObject;

/**
 * Real.asin.
 *
 * @since 0.40
 * @checkstyle TypeNameCheck (100 lines)
 */
@XmirObject(oname = "real.asin")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOreal$EOasin extends PhDefault implements Atom {
    @Override
    public Phi lambda() {
        return new ToPhi(
            Math.asin(new Dataized(this.take(Attr.RHO)).asNumber())
        );
    }
}
