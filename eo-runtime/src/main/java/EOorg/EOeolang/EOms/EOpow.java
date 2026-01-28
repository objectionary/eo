/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

/*
 * @checkstyle PackageNameCheck (4 lines)
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package EOorg.EOeolang.EOms; // NOPMD

import org.eolang.AtVoid;
import org.eolang.Atom;
import org.eolang.Dataized;
import org.eolang.PhDefault;
import org.eolang.Phi;
import org.eolang.XmirObject;

/**
 * Real.pow.
 *
 * @since 0.40
 * @checkstyle TypeNameCheck (100 lines)
 */
@XmirObject(oname = "pow")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOpow extends PhDefault implements Atom {
    /**
     * Ctor.
     */
    @SuppressWarnings("PMD.ConstructorOnlyInitializesOrCallOtherConstructors")
    public EOpow() {
        this.add("num", new AtVoid("num"));
        this.add("x", new AtVoid("x"));
    }

    @Override
    public Phi lambda() {
        return new ToPhi(
            Math.pow(
                new Dataized(this.take("num")).asNumber(),
                new Dataized(this.take("x")).asNumber()
            )
        );
    }
}
