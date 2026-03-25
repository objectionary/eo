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
 * The i16.as-i32.
 * @since 0.40
 * @checkstyle TypeNameCheck (6 lines)
 */
@XmirObject(oname = "i16.as-i32")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOi16$EOas_i32 extends PhDefault implements Atom {
    @Override
    public Phi lambda() {
        final Phi num = Phi.Φ.take("i32").copy();
        num.put(
            0,
            new ToPhi(
                new BytesOf(
                    new Dataized(this.take(Phi.RHO)).take(Short.class).intValue()
                ).take()
            )
        );
        return num;
    }
}
