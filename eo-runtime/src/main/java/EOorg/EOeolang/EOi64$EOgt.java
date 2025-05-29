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
import org.eolang.Dataized;
import org.eolang.PhDefault;
import org.eolang.PhVoid;
import org.eolang.Phi;
import org.eolang.XmirObject;

/**
 * The i64.gt object.
 *
 * @since 0.39.0
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "i64.gt")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOi64$EOgt extends PhDefault implements Atom {
    /**
     * Ctor.
     */
    @SuppressWarnings("PMD.ConstructorOnlyInitializesOrCallOtherConstructors")
    public EOi64$EOgt() {
        this.add("x", new PhVoid("x"));
    }

    @Override
    public Phi lambda() {
        return new ToPhi(
            new Dataized(this.take(Phi.RHO)).take(Long.class)
                > new Dataized(this.take("x").take("as-i64")).take(Long.class)
        );
    }
}
