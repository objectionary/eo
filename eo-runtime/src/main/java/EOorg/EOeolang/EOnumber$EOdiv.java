/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

/*
 * @checkstyle PackageNameCheck (4 lines)
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package EOorg.EOeolang; // NOPMD

import org.eolang.AtVoid;
import org.eolang.Atom;
import org.eolang.Attr;
import org.eolang.Data;
import org.eolang.Expect;
import org.eolang.PhDefault;
import org.eolang.Phi;
import org.eolang.XmirObject;

/**
 * Number.div object.
 *
 * @since 0.39.0
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "number.div")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOnumber$EOdiv extends PhDefault implements Atom {
    /**
     * Ctor.
     */
    @SuppressWarnings("PMD.ConstructorOnlyInitializesOrCallOtherConstructors")
    public EOnumber$EOdiv() {
        this.add("x", new AtVoid("x"));
    }

    @Override
    public Phi lambda() {
        final Double left = new Expect.Number(Expect.at(this, Attr.RHO)).it();
        final Double right = new Expect.Number(Expect.at(this, "x")).it();
        return new Data.ToPhi(left / right);
    }
}
