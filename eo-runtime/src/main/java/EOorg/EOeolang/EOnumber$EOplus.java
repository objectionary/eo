/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

/*
 * @checkstyle PackageNameCheck (4 lines)
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package EOorg.EOeolang; // NOPMD

import org.eolang.AtVoid;
import org.eolang.Atom;
import org.eolang.Data;
import org.eolang.Expect;
import org.eolang.PhDefault;
import org.eolang.Phi;
import org.eolang.XmirObject;

/**
 * Number.plus.
 *
 * @since 0.39.0
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "number.plus")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOnumber$EOplus extends PhDefault implements Atom {
    /**
     * Ctor.
     */
    @SuppressWarnings("PMD.ConstructorOnlyInitializesOrCallOtherConstructors")
    public EOnumber$EOplus() {
        this.add("x", new AtVoid("x"));
    }

    @Override
    public Phi lambda() {
        return new Data.ToPhi(
            Double.sum(
                new Expect.Number(Expect.at(this, Phi.RHO)).it(),
                new Expect.Number(Expect.at(this, "x")).it()
            )
        );
    }
}
