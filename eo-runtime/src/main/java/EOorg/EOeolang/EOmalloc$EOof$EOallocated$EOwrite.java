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
import org.eolang.Dataized;
import org.eolang.PhDefault;
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
        this.add("offset", new AtVoid("offset"));
        this.add("data", new AtVoid("data"));
    }

    @Override
    public Phi lambda() {
        Heaps.INSTANCE.write(
            new Dataized(this.take(Attr.RHO).take("id")).asNumber().intValue(),
            new Dataized(this.take("offset")).asNumber().intValue(),
            new Dataized(this.take("data")).take()
        );
        return new Data.ToPhi(true);
    }
}
