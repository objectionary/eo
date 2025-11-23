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
 * Malloc.of.allocated.write object.
 * @since 0.36.0
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "malloc.of.allocated.write")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOmalloc$EOof$EOallocated$EOwrite extends PhDefault implements Atom {
    /**
     * Ctor.
     */
    @SuppressWarnings("PMD.ConstructorOnlyInitializesOrCallOtherConstructors")
    EOmalloc$EOof$EOallocated$EOwrite() {
        this.add("offset", new PhVoid("offset"));
        this.add("data", new PhVoid("data"));
    }

    @Override
    public Phi lambda() {
        Heaps.INSTANCE.write(
            new Dataized(this.take(Phi.RHO).take("id")).asNumber().intValue(),
            new Dataized(this.take("offset")).asNumber().intValue(),
            new Dataized(this.take("data")).take()
        );
        return new Data.ToPhi(true);
    }
}
