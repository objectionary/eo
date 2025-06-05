/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

/*
 * @checkstyle PackageNameCheck (4 lines)
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package EOorg.EOeolang; // NOPMD

import org.eolang.Atom;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.PhDefault;
import org.eolang.PhVoid;
import org.eolang.Phi;
import org.eolang.XmirObject;

/**
 * BYTES.RIGHT.
 *
 * @since 0.1.0
 * @checkstyle TypeNameCheck (15 lines)
 */
@XmirObject(oname = "bytes.right")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EObytes$EOright extends PhDefault implements Atom {
    /**
     * Ctor.
     */
    @SuppressWarnings("PMD.ConstructorOnlyInitializesOrCallOtherConstructors")
    public EObytes$EOright() {
        this.add("x", new PhVoid("x"));
    }

    @Override
    public Phi lambda() {
        return new Data.ToPhi(
            new Dataized(this.take(Phi.RHO))
                .asBytes()
                .shift(new Dataized(this.take("x")).asNumber().intValue())
                .take()
        );
    }
}
