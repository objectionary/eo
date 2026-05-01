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
 * The i64.gt object.
 * @since 0.39.0
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "i64.gt")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOi64$EOgt extends PhDefault implements Atom {

    /**
     * Ctor.
     */
    public EOi64$EOgt() {
        super(new Attrs(new AttrEntry("x", new AtVoid("x"))));
    }

    @Override
    public Phi lambda() {
        return new Data.ToPhi(
            new Dataized(this.take(Phi.RHO)).take(Long.class)
                > new Dataized(this.take("x").take("as-i64")).take(Long.class)
        );
    }
}
