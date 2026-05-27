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
        super(new Attrs(new Attr("x", new AtVoid("x"))));
    }

    @Override
    public Phi lambda() {
        return new Data.ToPhi(
            new Expect.I64(Expect.at(this, Phi.RHO)).it()
            > new Expect.I64(Expect.at(this, "x")).it()
        );
    }
}
