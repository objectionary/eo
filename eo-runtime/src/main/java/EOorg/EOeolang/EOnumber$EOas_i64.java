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
import org.eolang.BytesOf;
import org.eolang.Data;
import org.eolang.Expect;
import org.eolang.PhDefault;
import org.eolang.Phi;
import org.eolang.XmirObject;

/**
 * The number.as-i64.
 * @since 0.40
 * @checkstyle TypeNameCheck (6 lines)
 */
@XmirObject(oname = "number.as-i64")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOnumber$EOas_i64 extends PhDefault implements Atom {
    @Override
    public Phi lambda() {
        final Phi num = Phi.Φ.take("org.eolang.i64").copy();
        num.put(
            0,
            new Data.ToPhi(
                new BytesOf(
                    new Expect.Number(Expect.at(this, Phi.RHO)).it().longValue()
                ).take()
            )
        );
        return num;
    }
}
