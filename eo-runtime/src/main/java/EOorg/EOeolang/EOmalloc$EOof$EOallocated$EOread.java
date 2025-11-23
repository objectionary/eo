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
 * Malloc.of.allocated.read object.
 * @since 0.36.0
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "malloc.of.allocated.read")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOmalloc$EOof$EOallocated$EOread extends PhDefault implements Atom {
    /**
     * Ctor.
     */
    @SuppressWarnings("PMD.ConstructorOnlyInitializesOrCallOtherConstructors")
    EOmalloc$EOof$EOallocated$EOread() {
        this.add("offset", new PhVoid("offset"));
        this.add("length", new PhVoid("length"));
    }

    @Override
    public Phi lambda() {
        return new Data.ToPhi(
            Heaps.INSTANCE.read(
                new Dataized(this.take(Attr.RHO).take("id")).asNumber().intValue(),
                new Dataized(this.take("offset")).asNumber().intValue(),
                new Dataized(this.take("length")).asNumber().intValue()
            )
        );
    }
}
