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
 * BYTES.OR.
 *
 * @since 0.1.0
 * @checkstyle TypeNameCheck (15 lines)
 */
@XmirObject(oname = "bytes.or")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EObytes$EOor extends PhDefault implements Atom {
    /**
     * Ctor.
     */
    @SuppressWarnings("PMD.ConstructorOnlyInitializesOrCallOtherConstructors")
    public EObytes$EOor() {
        this.add("b", new PhVoid("b"));
    }

    @Override
    public Phi lambda() {
        return new Data.ToPhi(
            new Dataized(this.take(Phi.RHO)).asBytes().or(
                new Dataized(this.take("b")).asBytes()
            ).take()
        );
    }
}
