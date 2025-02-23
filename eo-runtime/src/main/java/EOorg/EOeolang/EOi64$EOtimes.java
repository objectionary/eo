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
import org.eolang.BytesOf;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.PhDefault;
import org.eolang.Phi;
import org.eolang.XmirObject;

/**
 * The i64.times object.
 * @since 0.39.0
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "i64.times")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOi64$EOtimes extends PhDefault implements Atom {
    /**
     * Ctor.
     */
    @SuppressWarnings("PMD.ConstructorOnlyInitializesOrCallOtherConstructors")
    public EOi64$EOtimes() {
        this.add("x", new AtVoid("x"));
    }

    @Override
    public Phi lambda() {
        final Phi num = Phi.Φ.take("org.eolang.i64").copy();
        num.put(
            0,
            new Data.ToPhi(
                new BytesOf(
                    new Dataized(this.take(Attr.RHO)).take(Long.class)
                        * new Dataized(this.take("x").take("as-i64")).take(Long.class)
                ).take()
            )
        );
        return num;
    }
}
