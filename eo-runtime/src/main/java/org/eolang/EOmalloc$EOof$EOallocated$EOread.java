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
    public EOmalloc$EOof$EOallocated$EOread() {
        super(new Attrs(
            new Attr("offset", new AtVoid("offset")),
            new Attr("length", new AtVoid("length"))
        ));
    }

    @Override
    public Phi lambda() {
        return new Data.ToPhi(
            Heaps.INSTANCE.read(
                new Expect.Natural(Expect.at(this.take(Phi.RHO), "id")).it(),
                new Expect.Natural(Expect.at(this, "offset")).it(),
                new Expect.Natural(Expect.at(this, "length")).it()
            )
        );
    }
}
