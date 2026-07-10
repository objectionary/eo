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
 * The number.as-i64.
 * @since 0.40
 * @checkstyle TypeNameCheck (6 lines)
 */
@XmirObject(oname = "number.as-i64")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOnumber$EOas_i64 extends PhDefault implements Atom {

    @Override
    public Phi lambda() {
        final Phi num = Phi.Φ.take("i64").copy();
        num.put(
            0,
            new Data.ToPhi(
                new BytesOf(
                    Expect.at(this, Phi.RHO)
                        .that(phi -> new Dataized(phi).asNumber())
                        .otherwise("must be a number")
                        .must(number -> number >= Long.MIN_VALUE && number < 0x1p63)
                        .otherwise("must fit into long range")
                        .that(Double::longValue)
                        .it()
                ).take()
            )
        );
        return num;
    }
}
