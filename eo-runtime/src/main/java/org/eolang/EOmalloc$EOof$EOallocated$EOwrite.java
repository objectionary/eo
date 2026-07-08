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
 * Malloc.of.allocated.write object.
 * @since 0.36.0
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "malloc.of.allocated.write")
@Impure
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOmalloc$EOof$EOallocated$EOwrite extends PhDefault implements Atom {

    /**
     * Ctor.
     */
    public EOmalloc$EOof$EOallocated$EOwrite() {
        super(new Attrs(
            new Attr("offset", new AtVoid("offset")),
            new Attr("data", new AtVoid("data"))
        ));
    }

    @Override
    public Phi lambda() {
        Heaps.INSTANCE.write(
            new Expect.Natural(Expect.at(this.take(Phi.RHO), "id")).it(),
            new Expect.Natural(Expect.at(this, "offset")).it(),
            new Dataized(this.take("data")).take()
        );
        return new Data.ToPhi(true);
    }
}
