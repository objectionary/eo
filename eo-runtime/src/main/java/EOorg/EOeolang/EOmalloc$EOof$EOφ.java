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
import org.eolang.Attr;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.Expect;
import org.eolang.PhDefault;
import org.eolang.Phi;
import org.eolang.XmirObject;

/**
 * Malloc.of.φ object.
 * @since 0.36.0
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "malloc.of.@")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOmalloc$EOof$EOφ extends PhDefault implements Atom {
    @Override
    public Phi lambda() {
        final Phi rho = this.take(Attr.RHO);
        final int identifier = Heaps.INSTANCE.malloc(
            this, new Expect.Natural(Expect.at(rho, "size")).it()
        );
        final Phi res;
        try {
            final Phi allocated = rho.take("allocated");
            allocated.put("id", new Data.ToPhi((long) identifier));
            final Phi scope = rho.take("scope").copy();
            scope.put(0, allocated);
            res = new Data.ToPhi(new Dataized(scope).take());
        } finally {
            Heaps.INSTANCE.free(identifier);
        }
        return res;
    }
}
