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
import org.eolang.Expect;
import org.eolang.PhDefault;
import org.eolang.PhVoid;
import org.eolang.Phi;
import org.eolang.XmirObject;

/**
 * Number.times object.
 *
 * @since 0.39.0
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "number.times")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOnumber$EOtimes extends PhDefault implements Atom {
    /**
     * Ctor.
     */
    @SuppressWarnings("PMD.ConstructorOnlyInitializesOrCallOtherConstructors")
    public EOnumber$EOtimes() {
        this.add("x", new PhVoid("x"));
    }

    @Override
    public Phi lambda() {
        final Double left = new Expect.Number(Expect.at(this, Phi.RHO)).it();
        final Double right = new Expect.Number(Expect.at(this, "x")).it();
        return new Data.ToPhi(left * right);
    }
}
