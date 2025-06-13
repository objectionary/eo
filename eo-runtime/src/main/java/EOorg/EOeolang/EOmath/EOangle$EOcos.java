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
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.PhDefault;
import org.eolang.Phi;
import org.eolang.XmirObject;

/**
 * Angle.cos.
 *
 * @since 0.40
 * @checkstyle TypeNameCheck (100 lines)
 */
@XmirObject(oname = "angle.cos")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOangle$EOcos extends PhDefault implements Atom {
    @Override
    public Phi lambda() {
        return new Data.ToPhi(
            Math.cos(new Dataized(this.take(Phi.RHO)).asNumber())
        );
    }
}
