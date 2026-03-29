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
 * Malloc.of.allocated.size object.
 * @since 0.41.0
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "malloc.of.allocated.size")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOmalloc$EOof$EOallocated$EOsize extends PhDefault implements Atom {
    @Override
    public Phi lambda() {
        return new Data.ToPhi(
            Heaps.INSTANCE.size(
                new Dataized(this.take(Phi.RHO).take("id")).asNumber().intValue()
            )
        );
    }
}
