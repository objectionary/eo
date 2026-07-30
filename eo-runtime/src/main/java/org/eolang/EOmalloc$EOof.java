/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

/**
 * Malloc.of object.
 * @since 0.36.0
 * @checkstyle IllegalIdentifierNameCheck (6 lines)
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "malloc.of")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOmalloc$EOof extends PhDefault implements Atom {

    /**
     * Ctor.
     */
    public EOmalloc$EOof() {
        super(new Attrs(
            new Attr("size", new AtVoid("size")),
            new Attr("scope", new AtVoid("scope"))
        ));
    }

    @Override
    public Phi lambda() {
        final int identifier = Heaps.INSTANCE.malloc(
            this, new Expect.Natural(Expect.at(this, "size")).it()
        );
        final Phi res;
        try {
            final Phi chunk = Phi.Φ.take("chunk").copy();
            chunk.put("id", new Data.ToPhi((long) identifier));
            final Phi scope = this.take("scope").copy();
            scope.put(0, chunk);
            res = new Data.ToPhi(new Dataized(scope).take());
        } finally {
            Heaps.INSTANCE.free(identifier);
        }
        return res;
    }
}
