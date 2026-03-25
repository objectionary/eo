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
 * Number.gt object.
 *
 * @since 0.39.0
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "number.gt")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOnumber$EOgt extends PhDefault implements Atom {
    /**
     * Ctor.
     */
    @SuppressWarnings("PMD.ConstructorOnlyInitializesOrCallOtherConstructors")
    public EOnumber$EOgt() {
        this.add("x", new AtVoid("x"));
    }

    @Override
    @SuppressWarnings("PMD.UnnecessaryLocalRule")
    public Phi lambda() {
        final Double left = new Expect.Number(Expect.at(this, Phi.RHO)).it();
        final Double right = new Expect.Number(Expect.at(this, "x")).it();
        return new Data.ToPhi(left > right);
    }
}
