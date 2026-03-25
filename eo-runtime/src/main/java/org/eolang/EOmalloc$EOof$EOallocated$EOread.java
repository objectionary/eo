/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

/*
 * @checkstyle PackageNameCheck (4 lines)
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package org.eolang;

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
    public EOmalloc$EOof$EOallocated$EOread() {
        this.add("offset", new AtVoid("offset"));
        this.add("length", new AtVoid("length"));
    }

    @Override
    public Phi lambda() {
        return new Data.ToPhi(
            Heaps.INSTANCE.read(
                new Dataized(this.take(Phi.RHO).take("id")).asNumber().intValue(),
                new Dataized(this.take("offset")).asNumber().intValue(),
                new Dataized(this.take("length")).asNumber().intValue()
            )
        );
    }
}
