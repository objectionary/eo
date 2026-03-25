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
 * Number.floor object.
 *
 * @since 0.39.0
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "number.floor")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOnumber$EOfloor extends PhDefault implements Atom {
    @Override
    public Phi lambda() {
        return new Data.ToPhi(
            new Expect.Number(Expect.at(this, Phi.RHO)).it().longValue()
        );
    }
}
