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
    @SuppressWarnings("PMD.UnnecessaryLocalRule")
    public Phi lambda() {
        final Long left = new Expect.I64(Expect.at(this, Phi.RHO)).it();
        final Long right = new Expect.I64(Expect.at(this, "x")).it();
        return new Data.ToPhi(left > right);
    }
}
