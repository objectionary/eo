/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

/*
 * @checkstyle PackageNameCheck (4 lines)
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package EOorg.EOeolang; // NOPMD

import java.util.Arrays;
import org.eolang.Atom;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.PhDefault;
import org.eolang.PhVoid;
import org.eolang.Phi;
import org.eolang.XmirObject;

/**
 * BYTES.EQ.
 *
 * @since 0.1.0
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "bytes.eq")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EObytes$EOeq extends PhDefault implements Atom {
    /**
     * Ctor.
     */
    @SuppressWarnings("PMD.ConstructorOnlyInitializesOrCallOtherConstructors")
    public EObytes$EOeq() {
        this.add("b", new PhVoid("b"));
    }

    @Override
    public Phi lambda() {
        return new Data.ToPhi(
            Arrays.equals(
                new Dataized(
                    this.take("b").take("as-bytes")
                ).take(),
                new Dataized(this.take(Attr.RHO)).take()
            )
        );
    }
}
