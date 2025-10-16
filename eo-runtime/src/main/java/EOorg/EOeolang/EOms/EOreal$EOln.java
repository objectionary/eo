/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

/*
 * @checkstyle PackageNameCheck (4 lines)
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package EOorg.EOeolang.EOms; // NOPMD

import org.eolang.Atom;
import org.eolang.Dataized;
import org.eolang.PhDefault;
import org.eolang.Phi;
import org.eolang.XmirObject;

/**
 * Real.ln.
 *
 * @since 0.40
 * @checkstyle TypeNameCheck (100 lines)
 */
@XmirObject(oname = "real.ln")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOreal$EOln extends PhDefault implements Atom {
    @Override
    public Phi lambda() {
        return new ToPhi(
            Math.log(new Dataized(this.take(Phi.RHO)).asNumber())
        );
    }
}
